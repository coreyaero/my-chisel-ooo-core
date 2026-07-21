package mycpu

import chisel3._
import chisel3.util._

class RenameEngine extends Module {
    val io = IO(new Bundle {
        val flush       = Input(Bool())
        val br_resolve  = Input(new BranchResolve())

        // Dec 0
        val dec0_valid  = Input(Bool())
        val dec0_fire   = Input(Bool()) // ★ 新增：真正离开 ID 的握手
        val dec0_we     = Input(Bool())
        val dec0_raddr1 = Input(UInt(5.W))
        val dec0_raddr2 = Input(UInt(5.W))
        val dec0_waddr  = Input(UInt(5.W))
        val dec0_is_br  = Input(Bool())
        val dec0_ready  = Output(Bool())
        val dec0_psrc1  = Output(UInt(Config.prfPtrWidth.W))
        val dec0_psrc2  = Output(UInt(Config.prfPtrWidth.W))
        val dec0_pdest  = Output(UInt(Config.prfPtrWidth.W))
        val dec0_old_p  = Output(UInt(Config.prfPtrWidth.W))
        val dec0_br_tag = Output(UInt(2.W))
        val dec0_br_mask= Output(UInt(4.W))

        // Dec 1
        val dec1_valid  = Input(Bool())
        val dec1_fire   = Input(Bool()) // ★ 新增：真正离开 ID 的握手
        val dec1_we     = Input(Bool())
        val dec1_raddr1 = Input(UInt(5.W))
        val dec1_raddr2 = Input(UInt(5.W))
        val dec1_waddr  = Input(UInt(5.W))
        val dec1_is_br  = Input(Bool())
        val dec1_ready  = Output(Bool())
        val dec1_psrc1  = Output(UInt(Config.prfPtrWidth.W))
        val dec1_psrc2  = Output(UInt(Config.prfPtrWidth.W))
        val dec1_pdest  = Output(UInt(Config.prfPtrWidth.W))
        val dec1_old_p  = Output(UInt(Config.prfPtrWidth.W))
        val dec1_br_tag = Output(UInt(2.W))
        val dec1_br_mask= Output(UInt(4.W))

        // Commit Port (ROB 依然是单提交，不受影响)
        val commit_valid= Input(Bool())
        val commit_we   = Input(Bool())
        val commit_raddr= Input(UInt(5.W))
        val commit_paddr= Input(UInt(Config.prfPtrWidth.W))
        val commit_old_p= Input(UInt(Config.prfPtrWidth.W))

        // Commit Port 1 (新增)
        val commit1_valid= Input(Bool())
        val commit1_we   = Input(Bool())
        val commit1_raddr= Input(UInt(5.W))
        val commit1_paddr= Input(UInt(Config.prfPtrWidth.W))
        val commit1_old_p= Input(UInt(Config.prfPtrWidth.W))

        val current_lsq_tail = Input(UInt(4.W))
        val br_restore_tail  = Output(UInt(4.W))

        val dec0_need_lsq = Input(Bool())
    })

    val f_rat = RegInit(VecInit((0 until 32).map(_.U(Config.prfPtrWidth.W))))
    val c_rat = RegInit(VecInit((0 until 32).map(_.U(Config.prfPtrWidth.W))))

    val freeBitsVal = ((BigInt(1) << (Config.prfEntries - 32)) - 1) << 32
    val INIT_FREE_BITS = freeBitsVal.U(Config.prfEntries.W)

    val spec_free_bits   = RegInit(INIT_FREE_BITS) 
    val commit_free_bits = RegInit(INIT_FREE_BITS)

    val global_mask = RegInit(0.U(4.W))
    val snap_f_rat  = RegInit(VecInit(Seq.fill(4)(VecInit((0 until 32).map(_.U(Config.prfPtrWidth.W))))))
    val snap_free   = RegInit(VecInit(Seq.fill(4)(0.U(Config.prfEntries.W))))
    val snap_mask   = RegInit(VecInit(Seq.fill(4)(0.U(4.W)))) 
    val snap_lsq_tail = RegInit(VecInit(Seq.fill(4)(0.U(4.W))))

    // ==========================================
    // ★ 修复漏洞二：提前计算当拍的分支结算结果，释放面具
    // ==========================================
    val res_tag = io.br_resolve.tag
    val tag_bit = 1.U(4.W) << res_tag
    val mask_clear_bit = Mux(io.br_resolve.valid, ~tag_bit, "b1111".U(4.W))
    val current_clean_mask = global_mask & mask_clear_bit

    // ==========================================
    // 资源分配：使用净化后的面具 (current_clean_mask)
    // ==========================================
    val ALL_ONES = ~(0.U(Config.prfEntries.W))
    val ALL_ZEROS = 0.U(Config.prfEntries.W)

    val free_idx0 = PriorityEncoder(spec_free_bits)
    val spec_free_no_0 = spec_free_bits & ~UIntToOH(free_idx0, Config.prfEntries)
    val free_idx1 = PriorityEncoder(spec_free_no_0)
    val has_free0 = spec_free_bits.orR
    val has_free1 = spec_free_no_0.orR

    val free_tags = WireDefault(VecInit(Seq.fill(4)(false.B)))
    // ★ 这里改成用 current_clean_mask 取反！当拍释放，当拍复用！
    for (i <- 0 until 4) { free_tags(i) := !current_clean_mask(i) } 
    val tag0 = PriorityEncoder(free_tags.asUInt)(1, 0) // 强制截为 2 位
    val free_tags_no_0 = free_tags.asUInt & ~(1.U(4.W) << tag0)
    val tag1 = PriorityEncoder(free_tags_no_0)(1, 0)   // 强制截为 2 位
    val has_tag0 = free_tags.asUInt.orR
    val has_tag1 = free_tags_no_0.orR

    // ==========================================
    // 需求计算与分配路由 (谁用哪个寄存器)
    // ==========================================
    val need_reg0 = io.dec0_we && (io.dec0_waddr =/= 0.U)
    val need_reg1 = io.dec1_we && (io.dec1_waddr =/= 0.U)
    
    val pdest0 = Mux(need_reg0, free_idx0, 0.U)
    val pdest1 = Mux(need_reg1, Mux(need_reg0, free_idx1, free_idx0), 0.U) // 如果 0 没用，1 就可以占用第一个空闲
    val has_req_reg0 = !need_reg0 || has_free0
    val has_req_reg1 = !need_reg1 || Mux(need_reg0, has_free1, has_free0)

    val br_tag0 = Mux(io.dec0_is_br, tag0, 0.U)
    val br_tag1 = Mux(io.dec1_is_br, Mux(io.dec0_is_br, tag1, tag0), 0.U(2.W))(1, 0)
    val has_req_tag0 = !io.dec0_is_br || has_tag0
    val has_req_tag1 = !io.dec1_is_br || Mux(io.dec0_is_br, has_tag1, has_tag0)

    // ★ 直接无视物理寄存器的余量，秒出 Ready！
    io.dec0_ready := has_req_tag0
    io.dec1_ready := io.dec0_ready && has_req_tag1

    // ==========================================
    // ★ 组内依赖解析 (RAW 旁路神级短接)
    // ==========================================
    io.dec0_psrc1 := Mux(io.dec0_raddr1 === 0.U, 0.U, f_rat(io.dec0_raddr1))
    io.dec0_psrc2 := Mux(io.dec0_raddr2 === 0.U, 0.U, f_rat(io.dec0_raddr2))
    
    val rw_conflict1 = need_reg0 && (io.dec1_raddr1 === io.dec0_waddr)
    val rw_conflict2 = need_reg0 && (io.dec1_raddr2 === io.dec0_waddr)
    val ww_conflict  = need_reg0 && (io.dec1_waddr === io.dec0_waddr) // 罕见情况：两条指令写同一个寄存器

    io.dec1_psrc1 := Mux(io.dec1_raddr1 === 0.U, 0.U, Mux(rw_conflict1, pdest0, f_rat(io.dec1_raddr1)))
    io.dec1_psrc2 := Mux(io.dec1_raddr2 === 0.U, 0.U, Mux(rw_conflict2, pdest0, f_rat(io.dec1_raddr2)))

    io.dec0_pdest := pdest0
    io.dec1_pdest := pdest1
    io.dec0_old_p := Mux(!need_reg0, 0.U, f_rat(io.dec0_waddr))
    // 如果写冲突了，指令 1 替换掉的其实就是指令 0 刚刚分到的那个（刚出生就夭折了）
    io.dec1_old_p := Mux(!need_reg1, 0.U, Mux(ww_conflict, pdest0, f_rat(io.dec1_waddr)))

    io.dec0_br_tag := br_tag0
    io.dec1_br_tag := br_tag1

    // ==========================================
    // 状态流转控制
    // ==========================================
    val is_mispredict = io.br_resolve.valid && io.br_resolve.mispredict
    // ★ 核心修复：必须看到外面的 fire 信号（成功挤进 ROB/IQ），才允许消耗寄存器！
    val fire0 = io.dec0_fire && !is_mispredict
    val fire1 = io.dec1_fire && !is_mispredict

    val do_alloc0 = fire0 && need_reg0
    val do_alloc1 = fire1 && need_reg1
    val do_snap0  = fire0 && io.dec0_is_br
    val do_snap1  = fire1 && io.dec1_is_br
    
    // 面具分发
    io.dec0_br_mask := current_clean_mask
    val mask_alloc0_bit = Mux(do_snap0, 1.U(4.W) << br_tag0, 0.U(4.W))
    io.dec1_br_mask := current_clean_mask | mask_alloc0_bit
    val mask_alloc1_bit = Mux(do_snap1, 1.U(4.W) << br_tag1, 0.U(4.W))

    io.br_restore_tail := snap_lsq_tail(res_tag)

    val do_commit0 = io.commit_valid && io.commit_we && (io.commit_raddr =/= 0.U)
    val do_commit1 = io.commit1_valid && io.commit1_we && (io.commit1_raddr =/= 0.U)
    val commit_mask0 = Mux(do_commit0 && (io.commit_old_p =/= 0.U), UIntToOH(io.commit_old_p, Config.prfEntries), ALL_ZEROS)
    val commit_mask1 = Mux(do_commit1 && (io.commit1_old_p =/= 0.U), UIntToOH(io.commit1_old_p, Config.prfEntries), ALL_ZEROS)
    val combined_commit_mask = commit_mask0 | commit_mask1
    
    val next_c_rat = WireDefault(c_rat)
    when(do_commit0) { next_c_rat(io.commit_raddr) := io.commit_paddr }
    when(do_commit1) { next_c_rat(io.commit1_raddr) := io.commit1_paddr } // WAW 时后盖前

    val next_f_rat = WireDefault(f_rat)
    when(do_alloc0) { next_f_rat(io.dec0_waddr) := pdest0 }
    when(do_alloc1) { next_f_rat(io.dec1_waddr) := pdest1 }

    val alloc_mask0 = Mux(do_alloc0, ~UIntToOH(pdest0, Config.prfEntries), ALL_ONES)
    val alloc_mask1 = Mux(do_alloc1, ~UIntToOH(pdest1, Config.prfEntries), ALL_ONES)
    // 替换为 combined_commit_mask
    val normal_spec_free = (spec_free_bits & alloc_mask0 & alloc_mask1) | combined_commit_mask

    val commit_clear_mask0 = Mux(do_commit0 && (io.commit_paddr =/= 0.U), ~UIntToOH(io.commit_paddr, Config.prfEntries), ALL_ONES)
    val commit_clear_mask1 = Mux(do_commit1 && (io.commit1_paddr =/= 0.U), ~UIntToOH(io.commit1_paddr, Config.prfEntries), ALL_ONES)

    val next_commit_free  = (commit_free_bits & commit_clear_mask0 & commit_clear_mask1) | combined_commit_mask

    val next_snap_free = WireDefault(snap_free)
    val next_snap_mask = WireDefault(snap_mask)
    for (i <- 0 until 4) {
        // 替换为 combined_commit_mask
        next_snap_free(i) := snap_free(i) | combined_commit_mask 
        next_snap_mask(i) := snap_mask(i) & mask_clear_bit
    }

    when(do_snap0) {
        // 快照 0 必须抓取 alloc0 写入前的状态！
        val snap0_f = WireDefault(f_rat)
        when(do_alloc0) { snap0_f(io.dec0_waddr) := pdest0 }
        snap_f_rat(br_tag0) := snap0_f
        // 替换为 combined_commit_mask
        next_snap_free(br_tag0) := (spec_free_bits & alloc_mask0) | combined_commit_mask 
        next_snap_mask(br_tag0) := current_clean_mask
        snap_lsq_tail(br_tag0)  := io.current_lsq_tail 
    }
    when(do_snap1) {
        snap_f_rat(br_tag1) := next_f_rat
        next_snap_free(br_tag1) := normal_spec_free
        next_snap_mask(br_tag1) := current_clean_mask | mask_alloc0_bit
        
        // ★ 修复漏洞一：时间盲区补偿！
        // 如果 dec0 是访存指令，它当拍一定占了一个坑，dec1 的快照必须算上它！
        snap_lsq_tail(br_tag1)  := io.current_lsq_tail + io.dec0_need_lsq.asUInt 
    }

    when(io.flush) {
        // 彻底丢掉难读的 Mux 和 undefined 的 do_commit。
        // next_c_rat 已经完美包含了当拍双提交的 WAW 覆盖逻辑，直接拿来作为干净的恢复源！
        for (i <- 0 until 32) { next_f_rat(i) := next_c_rat(i) } 
        spec_free_bits := next_commit_free 
        global_mask    := 0.U(4.W)
    } .elsewhen(is_mispredict) {
        for (i <- 0 until 32) { next_f_rat(i) := snap_f_rat(res_tag)(i) }
        spec_free_bits := next_snap_free(res_tag) 
        global_mask    := next_snap_mask(res_tag) 
    } .otherwise {
        spec_free_bits := normal_spec_free
        global_mask    := current_clean_mask | mask_alloc0_bit | mask_alloc1_bit
    }

    snap_free        := next_snap_free
    snap_mask        := next_snap_mask
    f_rat            := next_f_rat
    c_rat            := next_c_rat
    commit_free_bits := next_commit_free
}
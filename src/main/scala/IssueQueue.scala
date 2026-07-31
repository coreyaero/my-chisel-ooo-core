package mycpu

import chisel3._
import chisel3.util._

class IssueQueue extends Module {
    val io = IO(new Bundle {
        val flush = Input(Bool())

        // 1. Dispatch 端口 0 
        val disp_valid  = Input(Bool())
        val disp_ready  = Output(Bool())
        val disp_data   = Input(new PipelineData())
        val psrc1       = Input(UInt(Config.prfPtrWidth.W))
        val psrc1_rdy   = Output(Bool())
        val psrc2       = Input(UInt(Config.prfPtrWidth.W))
        val psrc2_rdy   = Output(Bool())

        // 1. Dispatch 端口 1 (★ 新增)
        val disp1_valid  = Input(Bool())
        val disp1_ready  = Output(Bool())
        val disp1_data   = Input(new PipelineData())
        val psrc1_1      = Input(UInt(Config.prfPtrWidth.W))
        val psrc1_rdy_1  = Output(Bool())
        val psrc2_1      = Input(UInt(Config.prfPtrWidth.W))
        val psrc2_rdy_1  = Output(Bool())

        // 2. 乱序多发射端口
        val issue_alu0 = Decoupled(new PipelineData())
        val issue_alu1 = Decoupled(new PipelineData())
        val issue_mdu  = Decoupled(new PipelineData())
        val issue_agu  = Decoupled(new PipelineData())

        // 3. 双路 CDB 广播监听端口 
        val cdb0_valid  = Input(Bool())
        val cdb0_pdest  = Input(UInt(Config.prfPtrWidth.W))
        val cdb1_valid  = Input(Bool())
        val cdb1_pdest  = Input(UInt(Config.prfPtrWidth.W))

        val br_resolve  = Input(new BranchResolve())

        // ★★★ 新增：接收提前唤醒，并向上输出 PRF 的真实完备状态 ★★★
        val early_wakeup = Flipped(Valid(UInt(Config.prfPtrWidth.W)))
        val prf_ready_state = Output(Vec(Config.prfEntries, Bool()))

        // ★ 新增：ALU 提前唤醒探针
        val alu0_wakeup  = Flipped(Valid(UInt(Config.prfPtrWidth.W)))
        val alu1_wakeup  = Flipped(Valid(UInt(Config.prfPtrWidth.W)))
    })

    val prf_ready = RegInit(VecInit(Seq.fill(Config.prfEntries)(true.B)))

    // ★ 向 Top.scala 暴露 PRF 真实状态
    io.prf_ready_state := prf_ready

    // ★ 原有的 LSQ 唤醒
    val ew_valid = io.early_wakeup.valid && io.early_wakeup.bits =/= 0.U
    val ew_pdest = io.early_wakeup.bits

    // ★ 新增的 ALU 唤醒
    val ew0_valid = io.alu0_wakeup.valid && io.alu0_wakeup.bits =/= 0.U
    val ew0_pdest = io.alu0_wakeup.bits

    val ew1_valid = io.alu1_wakeup.valid && io.alu1_wakeup.bits =/= 0.U
    val ew1_pdest = io.alu1_wakeup.bits

    // ★ 封装一个绝妙的 Ready 判定函数
    def check_rdy(psrc_rdy: Bool, psrc: UInt): Bool = {
        psrc_rdy ||
        (io.cdb0_valid && psrc === io.cdb0_pdest) ||
        (io.cdb1_valid && psrc === io.cdb1_pdest) ||
        (ew_valid      && psrc === ew_pdest)      ||
        (ew0_valid     && psrc === ew0_pdest)     ||
        (ew1_valid     && psrc === ew1_pdest)
    }

    io.psrc1_rdy := check_rdy(prf_ready(io.psrc1), io.psrc1)
    io.psrc2_rdy := check_rdy(prf_ready(io.psrc2), io.psrc2)
    io.psrc1_rdy_1 := check_rdy(prf_ready(io.psrc1_1), io.psrc1_1)
    io.psrc2_rdy_1 := check_rdy(prf_ready(io.psrc2_1), io.psrc2_1)

    class IqEntry extends Bundle {
        val valid     = Bool()
        val psrc1     = UInt(Config.prfPtrWidth.W)
        val psrc1_rdy = Bool()
        val psrc2     = UInt(Config.prfPtrWidth.W)
        val psrc2_rdy = Bool()
        val data      = new PipelineData()
    }
    val iq = RegInit(VecInit(Seq.fill(Config.iqEntries)(0.U.asTypeOf(new IqEntry()))))

    // ==========================================
    // ★ 改造分配 (寻找 2 个空槽)
    // ==========================================
    val free_cands = WireDefault(VecInit(Seq.fill(Config.iqEntries)(false.B)))
    for (i <- 0 until Config.iqEntries) { free_cands(i) := !iq(i).valid }
    
    val has_free0 = free_cands.asUInt.orR
    val alloc_idx0 = PriorityEncoder(free_cands)
    
    // 把第 0 个槽位涂黑，继续找第 1 个空槽
    val free_cands_no_0 = free_cands.asUInt & ~(1.U(Config.iqEntries.W) << alloc_idx0)
    val has_free1 = free_cands_no_0.orR
    val alloc_idx1 = PriorityEncoder(free_cands_no_0)(Config.iqPtrWidth - 1, 0)

    io.disp_ready  := has_free0
    io.disp1_ready := has_free1

    val real_disp0 = io.disp_valid && !(io.br_resolve.valid && io.br_resolve.mispredict)
    val real_disp1 = io.disp1_valid && !(io.br_resolve.valid && io.br_resolve.mispredict)

    val fire0 = real_disp0 && has_free0
    val fire1 = real_disp1 && has_free1 && fire0 // 0 不走，1 坚决不走！

    when(fire0) {
        iq(alloc_idx0).valid     := true.B
        iq(alloc_idx0).data      := io.disp_data
        iq(alloc_idx0).psrc1     := io.disp_data.psrc1
        iq(alloc_idx0).psrc1_rdy := (!io.disp_data.src1_read) || (io.disp_data.psrc1 === 0.U) || io.psrc1_rdy
        iq(alloc_idx0).psrc2     := io.disp_data.psrc2
        iq(alloc_idx0).psrc2_rdy := (!io.disp_data.src2_read) || (io.disp_data.psrc2 === 0.U) || io.psrc2_rdy
        
        val dest0 = io.disp_data.pdest
        when(io.disp_data.regWriteEn && dest0 =/= 0.U) {
            prf_ready(dest0) := false.B
        }
    }

    when(fire1) {
        iq(alloc_idx1).valid     := true.B
        iq(alloc_idx1).data      := io.disp1_data
        iq(alloc_idx1).psrc1     := io.disp1_data.psrc1
        
        // ★ 核心短接逻辑防线！
        // 如果指令 1 用的源操作数刚好是同拍指令 0 写的，它的 PRF Ready 状态不可信！必须强制为 False 乖乖等 CDB！
        val is_raw1 = io.disp_data.regWriteEn && (io.disp_data.pdest === io.disp1_data.psrc1) && (io.disp_data.pdest =/= 0.U)
        val is_raw2 = io.disp_data.regWriteEn && (io.disp_data.pdest === io.disp1_data.psrc2) && (io.disp_data.pdest =/= 0.U)

        iq(alloc_idx1).psrc1_rdy := (!io.disp1_data.src1_read) || (io.disp1_data.psrc1 === 0.U) || (io.psrc1_rdy_1 && !is_raw1)
        
        iq(alloc_idx1).psrc2     := io.disp1_data.psrc2
        iq(alloc_idx1).psrc2_rdy := (!io.disp1_data.src2_read) || (io.disp1_data.psrc2 === 0.U) || (io.psrc2_rdy_1 && !is_raw2)
        
        val dest1 = io.disp1_data.pdest
        when(io.disp1_data.regWriteEn && dest1 =/= 0.U) {
            prf_ready(dest1) := false.B
        }
    }

    // ==========================================
    // B. CDB 唤醒 & 面具净化
    // ==========================================
    when(io.br_resolve.valid) {
        val tag_bit = 1.U(4.W) << io.br_resolve.tag
        for (i <- 0 until Config.iqEntries) {
            when(iq(i).valid) {
                val is_dependent = (iq(i).data.branch_mask & tag_bit) =/= 0.U
                when(io.br_resolve.mispredict) {
                    when(is_dependent) { iq(i).valid := false.B }
                } .otherwise {
                    when(is_dependent) { iq(i).data.branch_mask := iq(i).data.branch_mask & ~tag_bit }
                }
            }
        }
    }
    
    val cdb0_write = io.cdb0_valid && io.cdb0_pdest =/= 0.U
    val cdb1_write = io.cdb1_valid && io.cdb1_pdest =/= 0.U
    when(cdb0_write) { prf_ready(io.cdb0_pdest) := true.B }
    when(cdb1_write) { prf_ready(io.cdb1_pdest) := true.B }

    for (i <- 0 until Config.iqEntries) {
        when(iq(i).valid) {
            // ★ 只要被天上飞的 CDB 或者任意一个 Early Wakeup (LSQ/ALU0/ALU1) 扫中，立刻锁定为 Ready
            val match1 = check_rdy(false.B, iq(i).psrc1)
            when(match1) { iq(i).psrc1_rdy := true.B }

            val match2 = check_rdy(false.B, iq(i).psrc2)
            when(match2) { iq(i).psrc2_rdy := true.B }
        }
    }

    // ========================================================
    // C. 智能路由与乱序多发射 (极致时序优化版 V2 - 僵尸放行)
    // ========================================================
    val ready_vec  = WireDefault(VecInit(Seq.fill(Config.iqEntries)(false.B)))
    val is_mdu_vec = WireDefault(VecInit(Seq.fill(Config.iqEntries)(false.B)))
    val is_agu_vec = WireDefault(VecInit(Seq.fill(Config.iqEntries)(false.B)))
    val is_br_csr  = WireDefault(VecInit(Seq.fill(Config.iqEntries)(false.B)))

    // ★ 撤销了致命的 current_kill_mask，彻底解除 ALU0 对仲裁器的串行阻塞！

    for (i <- 0 until Config.iqEntries) {
        val d = iq(i).data
        
        is_mdu_vec(i) := d.resFromMulDiv
        is_agu_vec(i) := d.resFromMem || d.memWe || d.is_cacop || (d.tlbOp =/= TlbOp.NOP)
        is_br_csr(i)  := d.is_branch || d.isCsr || d.rdtimel || d.rdtimeh

        // ★ 修复方案：当拍的前馈唤醒 (Combinatorial Bypass Wakeup)
        // ★ 修复方案：当拍的前馈唤醒 (Combinatorial Bypass Wakeup)
        // 直接使用 check_rdy，囊括 CDB0, CDB1, LSQ 提前唤醒, 以及新增的两个 ALU 提前唤醒！
        val p1_rdy_now = check_rdy(iq(i).psrc1_rdy, iq(i).psrc1)
        val p2_rdy_now = check_rdy(iq(i).psrc2_rdy, iq(i).psrc2)

        // 使用当拍的组合逻辑状态进行仲裁
        ready_vec(i) := iq(i).valid && p1_rdy_now && p2_rdy_now
    }
    
    val ready_mask = ready_vec.asUInt

    val mdu_cands = ready_mask & is_mdu_vec.asUInt
    val has_mdu   = mdu_cands.orR
    val mdu_idx   = PriorityEncoder(mdu_cands)

    val agu_cands = ready_mask & is_agu_vec.asUInt
    val has_agu   = agu_cands.orR
    val agu_idx   = PriorityEncoder(agu_cands)

    val alu_all_cands = ready_mask & ~is_mdu_vec.asUInt & ~is_agu_vec.asUInt
    
    // ★ 智能分流：将候选人分为“特权指令(分支/CSR)”和“平民指令(普通ALU)”
    val br_cands  = alu_all_cands & is_br_csr.asUInt
    val reg_cands = alu_all_cands & ~is_br_csr.asUInt

    val has_br = br_cands.orR
    val br_idx = PriorityEncoder(br_cands)
    val br_oh  = PriorityEncoderOH(br_cands)

    val has_reg = reg_cands.orR
    val reg_idx = PriorityEncoder(reg_cands)
    val reg_oh  = PriorityEncoderOH(reg_cands)

    // ★ ALU0 仲裁：如果有分支，分支拥有绝对优先权霸占 ALU0；只有没分支时，平民才能用 ALU0
    val has_any_alu = has_br || has_reg
    val alu0_idx    = Mux(has_br, br_idx, reg_idx)
    val alu0_oh     = Mux(has_br, br_oh, reg_oh)

    // ★ ALU1 仲裁：ALU1 只能执行平民指令，且必须剔除刚刚已经占用了 ALU0 的那个幸运平民！
    val alu1_cands  = reg_cands & ~alu0_oh
    val has_alu1    = alu1_cands.orR
    val alu1_idx    = PriorityEncoder(alu1_cands)

    io.issue_alu0.valid := has_any_alu
    io.issue_alu0.bits  := iq(alu0_idx).data
    
    io.issue_alu1.valid := has_alu1
    io.issue_alu1.bits  := iq(alu1_idx).data

    io.issue_mdu.valid  := has_mdu
    io.issue_mdu.bits   := iq(mdu_idx).data

    io.issue_agu.valid  := has_agu
    io.issue_agu.bits   := iq(agu_idx).data

    when(io.issue_alu0.valid && io.issue_alu0.ready) { iq(alu0_idx).valid := false.B }
    when(io.issue_alu1.valid && io.issue_alu1.ready) { iq(alu1_idx).valid := false.B }
    when(io.issue_mdu.valid  && io.issue_mdu.ready)  { iq(mdu_idx).valid  := false.B }
    when(io.issue_agu.valid  && io.issue_agu.ready)  { iq(agu_idx).valid  := false.B }

    // (下方保留原有的 when(io.flush) 逻辑不变)

    // 这里保留你原本文件里的 when(io.flush) 逻辑即可

    when(io.flush) {
        for (i <- 0 until Config.iqEntries) { iq(i).valid := false.B }
        for (i <- 0 until Config.prfEntries) { prf_ready(i) := true.B }
    }
}
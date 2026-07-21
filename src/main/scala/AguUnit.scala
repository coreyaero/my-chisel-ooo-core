package mycpu

import chisel3._
import chisel3.util._

class AguUnit extends Module {
    val io = IO(new Bundle {
        val in  = Flipped(Decoupled(new PipelineData()))
        val out = Decoupled(new PipelineData())

        val flush = Input(Bool())
        val br_resolve_in = Input(new BranchResolve())

        val data_sram  = new SramIo()
        val data_uncached = Output(Bool())

        val mmu_config      = Input(new MmuConfig())
        val tlb_s1_vppn     = Output(UInt(19.W))
        val tlb_s1_va_bit12 = Output(Bool())
        val tlb_s1_asid     = Output(UInt(10.W))
        val tlb_s1_found    = Input(Bool())
        val tlb_s1_index    = Input(UInt(4.W))  
        val tlb_s1_ppn      = Input(UInt(20.W))
        val tlb_s1_ps       = Input(UInt(6.W))
        val tlb_s1_plv      = Input(UInt(2.W))  
        val tlb_s1_mat      = Input(UInt(2.W))  
        val tlb_s1_d        = Input(Bool())    
        val tlb_s1_v        = Input(Bool())    

        val invtlb_valid = Output(Bool())
        val invtlb_op    = Output(UInt(5.W))

        val cacop_en = Output(Bool())
        val cacop_op = Output(UInt(2.W)) // 提取高两位传给 Cache
        val cacop_is_icache = Output(Bool()) // 判定是不是针对 ICache
    })

    val valid_reg = RegInit(false.B)
    val data_reg  = RegInit(0.U.asTypeOf(new PipelineData()))

    // ==========================================
    // 1. 面具净化与爆破 (保持原样)
    // ==========================================
    val current_is_killed = valid_reg && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((data_reg.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    val real_valid = valid_reg && !current_is_killed

    //这里得赋值放下面去


    // ==========================================
    // 2. 原 StageEX 逻辑：地址算术与 TLB 翻译
    // ==========================================
    val src1_fwd = data_reg.src1_value
    val src2_fwd = data_reg.src2_value
    val alu_src1 = Mux(data_reg.src1IsPC, data_reg.pc, src1_fwd)
    val alu_src2 = Mux(data_reg.src2IsImm, data_reg.imm, Mux(data_reg.src2IsFour, 4.U, src2_fwd))
    val va = alu_src1 + alu_src2

    val is_tlbsrch = data_reg.tlbOp === TlbOp.SRCH
    val is_invtlb  = data_reg.tlbOp === TlbOp.INV

    io.tlb_s1_vppn := Mux(is_invtlb,  src2_fwd(31, 13), Mux(is_tlbsrch, io.mmu_config.tlbehi.vppn, va(31, 13)))
    io.tlb_s1_va_bit12 := va(12)
    io.tlb_s1_asid := Mux(is_invtlb,  src1_fwd(9, 0), io.mmu_config.asid.asid)

    io.invtlb_valid := is_invtlb && real_valid && !data_reg.hasException
    io.invtlb_op    := data_reg.invtlb_op

    val tlbsrch_res = Cat(!io.tlb_s1_found, 0.U(27.W), io.tlb_s1_index)
    val tlbsrch_mask = Mux(io.tlb_s1_found, "h8000000F".U(32.W), "h80000000".U(32.W))

    // DMW 命中判定与物理地址
    val dmw0_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw0.vseg) &&
               ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw0.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw0.plv3 === 1.U))
    val dmw1_hit = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && (va(31, 29) === io.mmu_config.dmw1.vseg) &&
                ((io.mmu_config.crmd.plv === 0.U && io.mmu_config.dmw1.plv0 === 1.U) || (io.mmu_config.crmd.plv === 3.U && io.mmu_config.dmw1.plv3 === 1.U))
    val dmw_hit = dmw0_hit || dmw1_hit
    val dmw_pa  = Mux(dmw0_hit, Cat(io.mmu_config.dmw0.pseg, va(28, 0)), Cat(io.mmu_config.dmw1.pseg, va(28, 0)))
    val tlb_pa = Mux(io.tlb_s1_ps === 12.U, Cat(io.tlb_s1_ppn, va(11, 0)), Cat(io.tlb_s1_ppn(19, 9), va(20, 0)))

    val pa = Mux((io.mmu_config.crmd.da === 1.U) && (io.mmu_config.crmd.pg === 0.U), va,
         Mux(dmw_hit, dmw_pa, Mux(io.tlb_s1_found && io.tlb_s1_v, tlb_pa, va)))
         
    // ★ 修复：操作类型是低 3 位！Hit Invalidate 是 2.U
    val cacop_is_hit_inval = data_reg.is_cacop && (data_reg.cacop_op(4, 3) === 2.U)
    val cacop_is_index     = data_reg.is_cacop && (data_reg.cacop_op(4, 3) =/= 2.U)
    

    val dmw_mat = Mux(dmw0_hit, io.mmu_config.dmw0.mat, io.mmu_config.dmw1.mat)
    val current_mat = Mux((io.mmu_config.crmd.da === 1.U) && (io.mmu_config.crmd.pg === 0.U), io.mmu_config.crmd.datm,
                      Mux(dmw_hit, dmw_mat, io.tlb_s1_mat))
    io.data_uncached := (current_mat === 0.U)

    // ==========================================
    // 3. 终极扁平化：早晚分离的异常判定 (Late Signal Injection)
    // ==========================================
    val isWord = data_reg.lsOp === LsOp.LD_W || data_reg.lsOp === LsOp.ST_W
    val isHalf = data_reg.lsOp === LsOp.LD_H || data_reg.lsOp === LsOp.LD_HU || data_reg.lsOp === LsOp.ST_H
    
    // 1. ALE 判定 (依赖 VA 低位)
    val ale = (data_reg.resFromMem || data_reg.memWe) && valid_reg && 
              ((isWord && (va(1, 0) =/= 0.U)) || (isHalf && va(0) === 1.U))

    // ★ 2. 提取早到信号 (Early Signals)：斩断对 ale 和 hasException 的依赖！
    val early_is_load  = data_reg.resFromMem && valid_reg
    val early_is_store = data_reg.memWe && valid_reg
    val early_hit_inv  = cacop_is_hit_inval && valid_reg
    val early_is_ls    = early_is_load || early_is_store || early_hit_inv
    
    val is_mapped = (io.mmu_config.crmd.pg === 1.U) && (io.mmu_config.crmd.da === 0.U) && !dmw_hit
    val early_is_mapped = is_mapped && early_is_ls

    // ★ 3. 提取迟到信号 (Late Signals)：全屏等待 TLB 查表结果
    val tlb_f = io.tlb_s1_found
    val tlb_v = io.tlb_s1_v
    val tlb_d = io.tlb_s1_d
    val priv_fault = (io.mmu_config.crmd.plv === 3.U) && (io.tlb_s1_plv === 0.U)

    // ★ 4. TLB 异常布尔判定 (抛弃串行逻辑，用 1 个 LUT6 瞬间拍平！)
    val raw_tlb_exc = !tlb_f || !tlb_v || priv_fault || (!tlb_d && early_is_store)
    val ex_mmu_exc = early_is_mapped && raw_tlb_exc

    val final_has_exc = data_reg.hasException || ale || ex_mmu_exc
// =========================================================
    // 4. 算址完毕，移交 LSQ (彻底删除了握手与死等！)
    // =========================================================
    // is_mem 保持不变，用于 req，但我们不再让 wstrb 依赖它
    val is_mem = (data_reg.resFromMem || data_reg.memWe || data_reg.is_cacop) && real_valid && !final_has_exc
    
    io.data_sram.req   := is_mem && !io.flush
    io.data_sram.wr    := data_reg.memWe
    io.data_sram.size  := Mux(isWord, 2.U, Mux(isHalf, 1.U, 0.U))
    
    val stMaskB = "b0001".U(4.W) << va(1, 0)
    val stMaskH = Mux(va(1), "b1100".U(4.W), "b0011".U(4.W))
    val stMaskW = "b1111".U(4.W)
    
    // ★ 核心优化：wstrb 直接无脑计算！只依赖最低 2 位地址和静态指令类型，彻底抛弃 is_mem 和 !final_has_exc
    val base_wstrb = Mux(isWord, stMaskW, Mux(isHalf, stMaskH, stMaskB))
    io.data_sram.wstrb := Mux(data_reg.memWe && real_valid && !io.flush, base_wstrb, 0.U(4.W))
    
    val final_pa_high = Mux(cacop_is_index, va(31,12), pa(31,12))
    io.data_sram.addr := Cat(final_pa_high, va(11, 0))

    val wdata_b = Fill(4, src2_fwd(7, 0))
    val wdata_h = Fill(2, src2_fwd(15, 0))
    io.data_sram.wdata := Mux(isWord, src2_fwd, Mux(isHalf, wdata_h, wdata_b))

    // ★ AGU 彻底解放：只要没异常，算完当拍就拍屁股走人！
    val agu_done = true.B

    // ==========================================
    // 8. 终极打包与无漏网状态机
    // ==========================================
    // 门外判定：刚到门口的新指令，是不是刚好在这一拍被击毙了？
    val incoming_is_killed = io.in.valid && io.br_resolve_in.valid && io.br_resolve_in.mispredict && ((io.in.bits.branch_mask & (1.U(4.W) << io.br_resolve_in.tag)) =/= 0.U)
    // 真正能被放进屋的有效指令：必须是 valid 且没有被当场击毙！
    val accepted_valid = io.in.valid && !incoming_is_killed

    // 屋里判定：如果当前指令被杀，AGU 视为空闲，立刻允许新指令进！
    val allow_in = !valid_reg || current_is_killed || (agu_done && io.out.ready)
    io.in.ready := allow_in

    // ★ 终极时序无冲突状态机 (严禁使用 if-else 嵌套干扰接收)
    when(io.flush) {
        valid_reg := false.B
    } .elsewhen(allow_in) {
        // 门开了！如果新指令没事就存 true；如果新指令被杀了，直接存 false.B，当拍变空泡！
        valid_reg := accepted_valid 
    } .elsewhen(current_is_killed) {
        // 门没开，但屋里的老指令被杀了，原地变空泡
        valid_reg := false.B
    }
    
    when(io.in.valid && allow_in) { data_reg := io.in.bits }

    //挪到这里
    when(io.br_resolve_in.valid && !io.br_resolve_in.mispredict) {
        val clear_mask = ~(1.U(4.W) << io.br_resolve_in.tag)
        data_reg.branch_mask := Mux(io.in.valid && io.in.ready, io.in.bits.branch_mask, data_reg.branch_mask) & clear_mask
    }

    val out_data = WireDefault(data_reg) 
    
    // ★ 修复：AGU 不再负责数据回写，仅仅向外透传计算出的物理地址 (pa) 备用
    // ★ 修复 Bug：正常访存给 PA 让 LSQ 寻址；发生异常给 VA，让 ROB 去填 BADV 寄存器！
    // 提取你原本代码里就算好的 CACOP 专用地址
    val cacop_addr = Cat(final_pa_high, va(11, 0))

    // =====================================================================
    // ★ 终极时序切割：彻底剥离 TLB 与 CDB 广播网络的纠缠！
    // =====================================================================
    // 1. 如果发生异常，ROB 需要的是 badv (也就是未翻译的 va)
    // 2. 如果是 TLBSRCH，需要的是 tlbsrch_res
    // 3. 正常 Load/Store/CACOP 指令，AGU 根本不会往 PRF 写数据！
    // 所以，我们直接无脑输出 VA，彻底剔除对 final_has_exc 和 pa 的依赖！
    // 这一刀直接斩断 24 级逻辑，拯救 3ns 以上的时序！
    out_data.ex_result := Mux(is_tlbsrch, tlbsrch_res, va)
    out_data.aux_data  := Mux(is_tlbsrch, tlbsrch_mask, 0.U)
    
    out_data.hasException := final_has_exc

    // ★ 5. 迟到异常码提取 (无脑 OR 树并映射，复用迟到信号)
    val raw_tlb_code = Mux(!tlb_f,                                              "h3F".U(6.W), 0.U) |
                       Mux(tlb_f && tlb_v && priv_fault,                        "h07".U(6.W), 0.U) |
                       Mux(tlb_f && !tlb_v && (early_is_load || early_hit_inv), "h01".U(6.W), 0.U) |
                       Mux(tlb_f && !tlb_v && early_is_store,                   "h02".U(6.W), 0.U) |
                       Mux(tlb_f && tlb_v && !priv_fault && !tlb_d && early_is_store, "h04".U(6.W), 0.U)

    val mmu_code = Mux(early_is_mapped, raw_tlb_code, 0.U)
    val final_ecode = Mux(ale, "h09".U(6.W), mmu_code)
    
    out_data.ecode := Mux(data_reg.hasException, data_reg.ecode, final_ecode)

    out_data.is_cacop  := Mux(real_valid, data_reg.is_cacop, false.B) 
    out_data.cacop_op  := Mux(real_valid, data_reg.cacop_op, 0.U(5.W))

    // ★ 净化为纯粹的静态指令属性，把握手任务全权交回给 io.data_sram.req
    val is_doing_cacop = real_valid && data_reg.is_cacop && !final_has_exc
    io.cacop_en := is_doing_cacop
    io.cacop_op := data_reg.cacop_op(4, 3) 
    io.cacop_is_icache := is_doing_cacop && (data_reg.cacop_op(2, 0) === 0.U)

    io.out.valid := valid_reg && agu_done
    io.out.bits  := out_data
}
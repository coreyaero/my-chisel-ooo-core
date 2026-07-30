package mycpu

import chisel3._
import chisel3.util._

class core_top extends RawModule {
    val aclk    = IO(Input(Clock()))
    val aresetn = IO(Input(Bool()))

    val intrpt = IO(Input(UInt(8.W)))

    val break_point = IO(Input(Bool()))
    val infor_flag  = IO(Input(Bool()))
    val reg_num     = IO(Input(UInt(5.W)))
    val ws_valid    = IO(Output(Bool()))
    val rf_rdata    = IO(Output(UInt(32.W)))

    // 1. AXI 接口定义保持完全不变...
    val arid    = IO(Output(UInt(4.W)))
    val araddr  = IO(Output(UInt(32.W)))
    val arlen   = IO(Output(UInt(8.W)))
    val arsize  = IO(Output(UInt(3.W)))
    val arburst = IO(Output(UInt(2.W)))
    val arlock  = IO(Output(UInt(2.W)))
    val arcache = IO(Output(UInt(4.W)))
    val arprot  = IO(Output(UInt(3.W)))
    val arvalid = IO(Output(Bool()))
    val arready = IO(Input(Bool()))
    val rid     = IO(Input(UInt(4.W)))
    val rdata   = IO(Input(UInt(32.W)))
    val rresp   = IO(Input(UInt(2.W)))
    val rlast   = IO(Input(Bool()))
    val rvalid  = IO(Input(Bool()))
    val rready  = IO(Output(Bool()))
    val awid    = IO(Output(UInt(4.W)))
    val awaddr  = IO(Output(UInt(32.W)))
    val awlen   = IO(Output(UInt(8.W)))
    val awsize  = IO(Output(UInt(3.W)))
    val awburst = IO(Output(UInt(2.W)))
    val awlock  = IO(Output(UInt(2.W)))
    val awcache = IO(Output(UInt(4.W)))
    val awprot  = IO(Output(UInt(3.W)))
    val awvalid = IO(Output(Bool()))
    val awready = IO(Input(Bool()))
    val wid     = IO(Output(UInt(4.W)))
    val wdata   = IO(Output(UInt(32.W)))
    val wstrb   = IO(Output(UInt(4.W)))
    val wlast   = IO(Output(Bool()))
    val wvalid  = IO(Output(Bool()))
    val wready  = IO(Input(Bool()))
    val bid     = IO(Input(UInt(4.W)))
    val bresp   = IO(Input(UInt(2.W)))
    val bvalid  = IO(Input(Bool()))
    val bready  = IO(Output(Bool()))

    val debug0_wb_pc       = IO(Output(UInt(32.W)))
    val debug0_wb_rf_wen   = IO(Output(UInt(4.W)))
    val debug0_wb_rf_wnum  = IO(Output(UInt(5.W)))
    val debug0_wb_rf_wdata = IO(Output(UInt(32.W)))

    val debug1_wb_pc       = IO(Output(UInt(32.W)))
    val debug1_wb_rf_wen   = IO(Output(UInt(4.W)))
    val debug1_wb_rf_wnum  = IO(Output(UInt(5.W)))
    val debug1_wb_rf_wdata = IO(Output(UInt(32.W)))

    val probe_cdb0_pc = IO(Output(UInt(32.W)))
    val probe_cdb1_pc = IO(Output(UInt(32.W)))

    val debug0_wb_valid = IO(Output(Bool()))
    val debug1_wb_valid = IO(Output(Bool()))

    
    val reset_high = (!aresetn).asAsyncReset

    withClockAndReset(aclk, reset_high) {

        //==========================================
        // Frontend
        //==========================================
        val if_module       = Module(new StageIF())
        val fetch_buf       = Module(new FetchBuffer(8))
        val id_module       = Module(new StageID())
        val disp_buf        = Module(new DispatchBuffer())

        val icfg = CacheConfig(enablePrefetch = true)
        val dcfg = CacheConfig(enablePrefetch = false)
        implicit val bridgeConfig = dcfg // Bridge 只需要用到行宽等通用配置，传 dcfg 即可
        val rename = Module(new RenameEngine())
        val prf    = Module(new PRF())
        val iq     = Module(new IssueQueue())
        val rob    = Module(new ROB())
        
        
        
        // ★ 核心替换：双发射乱序大心脏登场！
        val exec_engine = Module(new Exec())

        //==========================================
        // Flush logic
        //==========================================
        val flush_global    = rob.io.wb_flush
        val flush_branch    = exec_engine.io.branch_req
        val flush_frontend  = flush_global || flush_branch

        if_module.io.flush              := flush_frontend
        if_module.io.flush_target_pc    := Mux(flush_global, rob.io.wb_target_pc, exec_engine.io.branch_pc)
        fetch_buf.io.flush              := flush_frontend
        id_module.io.flush              := flush_frontend
        disp_buf.io.flush               := flush_frontend
        
        val csr        = Module(new CSR())
        val timer  = Module(new StableCounter())
        val tlb_module = Module(new tlb())
        val bridge = Module(new SramToAxiBridge())
        val icache = Module(new Cache()(icfg))
        val dcache = Module(new Cache()(dcfg))

        // ---------------- BPU 训练大环路 ----------------
        if_module.io.bpu_update := exec_engine.io.bpu_update
        if_module.io.commit_bpu_update := rob.io.commit_bpu_update

        // ==========================================
        // 全局 MMU 与中断配置
        // ==========================================
        if_module.io.mmu_config     := csr.io.mmu_config
        
        csr.io.hw_int_in            := 0.U(8.W)

        // ---------------- TLB 连线 ----------------
        if_module.io.tlb_port   <> tlb_module.io.s0
        exec_engine.io.tlb_port <> tlb_module.io.s1
        
        tlb_module.io.invtlb_valid  := exec_engine.io.invtlb_valid
        tlb_module.io.invtlb_op     := exec_engine.io.invtlb_op
        
        // ---------------- 前端 IF -> ID (自适应双进单出缓冲) ----------------
        

        // 接收双发输入
        fetch_buf.io.in0 <> if_module.io.out0
        fetch_buf.io.in1 <> if_module.io.out1

        id_module.io.in <> fetch_buf.io.out

        // ---------------- ID -> Rename -> IQ/ROB ----------------
        
        

        disp_buf.io.in0 <> id_module.io.out0
        disp_buf.io.in1 <> id_module.io.out1

        val d0 = disp_buf.io.out0.bits
        val d1 = disp_buf.io.out1.bits

        // =================================================================
        // ★ 终极修复：延长一拍的时序隔离墙！
        // Rename 模块为了优化时序，把分支恢复延迟了一拍 (delayed_is_mispredict)。
        // 因此，Dispatch 必须等 Rename 完全恢复好之后的下一拍，才能放行新指令！
        // 否则新指令的重命名记录会被延迟的快照瞬间抹杀，引发严重的寄存器错位！
        // =================================================================
        val is_mispredict = exec_engine.io.br_resolve.valid && exec_engine.io.br_resolve.mispredict
        val delayed_mispredict = RegNext(is_mispredict && !flush_frontend, false.B)
        
        // ★ 将 delayed_mispredict 加入阻塞条件，关门打狗！
        val dispatch_block = flush_frontend || is_mispredict || delayed_mispredict
        
        val d0_valid = disp_buf.io.out0.valid && !dispatch_block
        val d1_valid = disp_buf.io.out1.valid && !dispatch_block

        // ★ 发射限制与 LSQ 保护 (下面保持不变)
        val need_lsq0 = d0.resFromMem || d0.memWe || d0.is_cacop
        // ...
        val need_lsq1 = d1.resFromMem || d1.memWe || d1.is_cacop
        val lsq_conflict = need_lsq0 && need_lsq1

        // Rename 连线 0
        rename.io.dec0_valid    := d0_valid
        rename.io.dec0_raddr1   := d0.src1_addr
        rename.io.dec0_raddr2   := d0.src2_addr
        rename.io.dec0_we       := d0.regWriteEn
        rename.io.dec0_waddr    := d0.destReg
        rename.io.dec0_is_br    := d0.is_branch
        rename.io.dec0_need_lsq := need_lsq0 // ★ 喂入 LSQ 防盲区信息

        // Rename 连线 1
        rename.io.dec1_valid    := d1_valid
        rename.io.dec1_raddr1   := d1.src1_addr
        rename.io.dec1_raddr2   := d1.src2_addr
        rename.io.dec1_we       := d1.regWriteEn
        rename.io.dec1_waddr    := d1.destReg
        rename.io.dec1_is_br    := d1.is_branch

        // 控制信号
        rename.io.flush      := flush_global
        rename.io.br_resolve := exec_engine.io.br_resolve

       // ★ 分配时必须同时看 LSQ 和所有模块是否有空位
        val can_disp0 = rob.io.alloc_ready && iq.io.disp_ready && rename.io.dec0_ready && (!need_lsq0 || exec_engine.io.lsq_alloc.req.ready)
        val can_disp1 = can_disp0 && rob.io.alloc1_ready && iq.io.disp1_ready && rename.io.dec1_ready && (!need_lsq1 || exec_engine.io.lsq_alloc.req.ready) && !lsq_conflict

        // ★ 反向握手：告诉 DispatchBuffer 可以弹出几个
        //这么改没屁用，没屁用！我禁止你这么改！
        //disp_buf.io.out0.ready := can_disp0 && !dispatch_block
        //disp_buf.io.out1.ready := can_disp1 && !dispatch_block
        disp_buf.io.out0.ready := can_disp0
        disp_buf.io.out1.ready := can_disp1

        // ★ 只有指令合法且真能分发出去，才告诉 Rename 扣减资源！
        rename.io.dec0_fire := d0_valid && can_disp0
        rename.io.dec1_fire := d1_valid && can_disp1

        // ★ 核心修复：防止当拍解算的 branch_mask 污染新分发的指令 (面具重用误杀)
        val current_br_clear = Mux(exec_engine.io.br_resolve.valid && !exec_engine.io.br_resolve.mispredict, 
                                   ~(1.U(4.W) << exec_engine.io.br_resolve.tag), 
                                   "b1111".U(4.W))

        // ★ 向 LSQ 申请坑位 (动态多路复用)
        val real_need_lsq0 = d0_valid && need_lsq0 && can_disp0
        val real_need_lsq1 = d1_valid && need_lsq1 && can_disp1
        
        exec_engine.io.lsq_alloc.req.valid := real_need_lsq0 || real_need_lsq1
        
        exec_engine.io.lsq_alloc.req.bits.req_type := Mux(real_need_lsq0, Mux(d0.resFromMem, 0.U, Mux(d0.memWe, 1.U, 2.U)), Mux(d1.resFromMem, 0.U, Mux(d1.memWe, 1.U, 2.U)))
        exec_engine.io.lsq_alloc.req.bits.rob      := Mux(real_need_lsq0, rob.io.alloc_idx, rob.io.alloc1_idx)
        exec_engine.io.lsq_alloc.req.bits.pc       := Mux(real_need_lsq0, d0.pc, d1.pc)
        exec_engine.io.lsq_alloc.req.bits.pdest    := Mux(real_need_lsq0, rename.io.dec0_pdest, rename.io.dec1_pdest)
        
        // ★ 修复：给送进 LSQ 的 mask 洗净！
        exec_engine.io.lsq_alloc.req.bits.mask     := Mux(real_need_lsq0, rename.io.dec0_br_mask, rename.io.dec1_br_mask) & current_br_clear
        exec_engine.io.lsq_alloc.req.bits.cacop    := Mux(real_need_lsq0, d0.cacop_op, d1.cacop_op)
        exec_engine.io.lsq_alloc.req.bits.lsOp     := Mux(real_need_lsq0, d0.lsOp, d1.lsOp)

        // ★ 组装 Rename 后的数据
        val renamed_d0 = WireDefault(d0)
        renamed_d0.psrc1 := rename.io.dec0_psrc1
        renamed_d0.psrc2 := rename.io.dec0_psrc2
        renamed_d0.pdest := rename.io.dec0_pdest
        renamed_d0.old_pdest := rename.io.dec0_old_p
        renamed_d0.rob_idx := rob.io.alloc_idx
        // ★ 修复：给送进 IQ 的 mask 洗净！
        renamed_d0.branch_mask := rename.io.dec0_br_mask & current_br_clear 
        renamed_d0.branch_tag := rename.io.dec0_br_tag
        renamed_d0.lsq_idx := exec_engine.io.lsq_alloc.idx
        

        val renamed_d1 = WireDefault(d1)
        renamed_d1.psrc1 := rename.io.dec1_psrc1
        renamed_d1.psrc2 := rename.io.dec1_psrc2
        renamed_d1.pdest := rename.io.dec1_pdest
        renamed_d1.old_pdest := rename.io.dec1_old_p
        renamed_d1.rob_idx := rob.io.alloc1_idx
        // ★ 修复：给送进 IQ 的 mask 洗净！
        renamed_d1.branch_mask := rename.io.dec1_br_mask & current_br_clear 
        renamed_d1.branch_tag := rename.io.dec1_br_tag
        renamed_d1.lsq_idx := exec_engine.io.lsq_alloc.idx

        // ================= IQ 连线 =================
        iq.io.flush      := flush_global
        iq.io.br_resolve := exec_engine.io.br_resolve

        iq.io.disp_valid := d0_valid && can_disp0
        iq.io.disp_data  := renamed_d0
        iq.io.psrc1      := rename.io.dec0_psrc1
        iq.io.psrc2      := rename.io.dec0_psrc2

        iq.io.disp1_valid:= d1_valid && can_disp1
        iq.io.disp1_data := renamed_d1
        iq.io.psrc1_1    := rename.io.dec1_psrc1
        iq.io.psrc2_1    := rename.io.dec1_psrc2

        // ================= ROB 连线 =================
        rob.io.flush         := flush_global
        rob.io.br_resolve    := exec_engine.io.br_resolve

        rob.io.alloc_valid   := d0_valid && can_disp0
        rob.io.alloc_pc      := d0.pc
        rob.io.alloc_we      := d0.regWriteEn
        rob.io.alloc_waddr   := d0.destReg
        rob.io.alloc_paddr   := rename.io.dec0_pdest
        rob.io.alloc_old_p   := rename.io.dec0_old_p
        // ★ 修复：给送进 ROB 的 mask 洗净！
        rob.io.alloc_br_mask := rename.io.dec0_br_mask & current_br_clear 

        rob.io.alloc1_valid  := d1_valid && can_disp1
        rob.io.alloc1_pc     := d1.pc
        rob.io.alloc1_we     := d1.regWriteEn
        rob.io.alloc1_waddr  := d1.destReg
        rob.io.alloc1_paddr  := rename.io.dec1_pdest
        rob.io.alloc1_old_p  := rename.io.dec1_old_p
        // ★ 修复：给送进 ROB 的 mask 洗净！
        rob.io.alloc1_br_mask:= rename.io.dec1_br_mask & current_br_clear

        // ---------------- 乱序发射 IQ -> ExecutionEngine (流水线切片) ----------------
        // ★ 核心大改：引入【发射流水段 (Issue Buffer)】，斩断 Wakeup-Select-Read 世纪大路径！
        
        // ★ 终极修复：给流水段套上 flush 冲刷保护！
        // 任何分支预测失败或异常引发的 wb_flush，都必须彻底清空这些暂存的“幽灵指令”！
        val issue_flush = reset_high.asBool || flush_global
        
        val iss_q_alu0 = Module(new IssueBuffer())
        val iss_q_alu1 = Module(new IssueBuffer())
        val iss_q_mdu  = Module(new IssueBuffer())
        val iss_q_agu  = Module(new IssueBuffer())

        iss_q_alu0.io.flush := issue_flush
        iss_q_alu0.io.br_resolve := exec_engine.io.br_resolve
        
        iss_q_alu1.io.flush := issue_flush
        iss_q_alu1.io.br_resolve := exec_engine.io.br_resolve
        
        iss_q_mdu.io.flush  := issue_flush
        iss_q_mdu.io.br_resolve  := exec_engine.io.br_resolve
        
        iss_q_agu.io.flush  := issue_flush
        iss_q_agu.io.br_resolve  := exec_engine.io.br_resolve

        // IQ 选出的赢家，当拍立刻存入发射流水段
        iss_q_alu0.io.enq <> iq.io.issue_alu0
        iss_q_alu1.io.enq <> iq.io.issue_alu1
        iss_q_mdu.io.enq  <> iq.io.issue_mdu
        iss_q_agu.io.enq  <> iq.io.issue_agu

        // 1. 操作数地址现在从流水段寄存器中拉出，打向 PRF (跨界布线彻底终结！)
        prf.io.raddr1 := iss_q_alu0.io.deq.bits.psrc1
        prf.io.raddr2 := iss_q_alu0.io.deq.bits.psrc2
        prf.io.raddr3 := iss_q_alu1.io.deq.bits.psrc1
        prf.io.raddr4 := iss_q_alu1.io.deq.bits.psrc2
        prf.io.raddr5 := iss_q_mdu.io.deq.bits.psrc1
        prf.io.raddr6 := iss_q_mdu.io.deq.bits.psrc2
        prf.io.raddr7 := iss_q_agu.io.deq.bits.psrc1
        prf.io.raddr8 := iss_q_agu.io.deq.bits.psrc2

        // 2. 劫持 ALU0，注入真实数据 (从流水段出队)
        val alu0_in_data = WireDefault(iss_q_alu0.io.deq.bits)
        alu0_in_data.src1_value := prf.io.rdata1
        alu0_in_data.src2_value := prf.io.rdata2
        exec_engine.io.in_alu0.valid := iss_q_alu0.io.deq.valid
        exec_engine.io.in_alu0.bits  := alu0_in_data
        iss_q_alu0.io.deq.ready      := exec_engine.io.in_alu0.ready

        // 3. 劫持 ALU1
        val alu1_in_data = WireDefault(iss_q_alu1.io.deq.bits)
        alu1_in_data.src1_value := prf.io.rdata3
        alu1_in_data.src2_value := prf.io.rdata4
        exec_engine.io.in_alu1.valid := iss_q_alu1.io.deq.valid
        exec_engine.io.in_alu1.bits  := alu1_in_data
        iss_q_alu1.io.deq.ready      := exec_engine.io.in_alu1.ready

        // 4. 劫持 MDU
        val mdu_in_data = WireDefault(iss_q_mdu.io.deq.bits)
        mdu_in_data.src1_value := prf.io.rdata5
        mdu_in_data.src2_value := prf.io.rdata6
        exec_engine.io.in_mdu.valid  := iss_q_mdu.io.deq.valid
        exec_engine.io.in_mdu.bits   := mdu_in_data
        iss_q_mdu.io.deq.ready       := exec_engine.io.in_mdu.ready

        // 5. 劫持 AGU
        val agu_in_data = WireDefault(iss_q_agu.io.deq.bits)
        agu_in_data.src1_value := prf.io.rdata7
        agu_in_data.src2_value := prf.io.rdata8
        exec_engine.io.in_agu.valid  := iss_q_agu.io.deq.valid
        exec_engine.io.in_agu.bits   := agu_in_data
        iss_q_agu.io.deq.ready       := exec_engine.io.in_agu.ready
        
        exec_engine.io.flush    := flush_global
        exec_engine.io.rob_head := rob.io.head_idx







        exec_engine.io.timer_in := timer.io.timer_out
        exec_engine.io.mmu_config   := csr.io.mmu_config







        // ★ 核心连线：CSR 读路径 (ALU0 发起)
        csr.io.raddr := exec_engine.io.csr_raddr
        exec_engine.io.csr_rdata := csr.io.readData

        // ---------------- 双路写回 CDB -> IQ, PRF, ROB ----------------
        val cdb0 = exec_engine.io.cdb0
        val cdb1 = exec_engine.io.cdb1

        iq.io.cdb0_valid := cdb0.valid && cdb0.bits.regWriteEn
        iq.io.cdb0_pdest := cdb0.bits.pdest
        iq.io.cdb1_valid := cdb1.valid && cdb1.bits.regWriteEn
        iq.io.cdb1_pdest := cdb1.bits.pdest

        // ★ PRF 双写端口完美接线！
        // 斩断 Exception 依赖！哪怕是异常的垃圾数据，直接写进 PRF 也是安全的，ROB flush 会解决一切！
        prf.io.we1    := cdb0.valid && cdb0.bits.regWriteEn
        prf.io.waddr1 := cdb0.bits.pdest
        prf.io.wdata1 := cdb0.bits.ex_result
        
        prf.io.we2    := cdb1.valid && cdb1.bits.regWriteEn
        prf.io.waddr2 := cdb1.bits.pdest
        prf.io.wdata2 := cdb1.bits.ex_result

        rob.io.cdb0 := cdb0
        rob.io.cdb1 := cdb1

        // ==========================================
        // ★ 修复：LSQ 跨模块核心连线补丁
        // ==========================================
        // 1. Rename <-> ExecEngine(LSQ) 的快照与回档连线
        rename.io.current_lsq_tail          := exec_engine.io.lsq_state.current_tail
        exec_engine.io.lsq_state.br_restore := rename.io.br_restore_tail

        // 2. ExecEngine(LSQ) -> ROB 的内存违例报警连线
        rob.io.lsq_violation_valid          := exec_engine.io.lsq_violation.valid
        rob.io.lsq_violation_rob            := exec_engine.io.lsq_violation.rob
        rob.io.lsq_violation_pc             := exec_engine.io.lsq_violation.pc

        // 3. ROB -> ExecEngine(LSQ) 的提交通知连线
        exec_engine.io.commit_mem.valid0    := rob.io.commit_mem_valid0
        exec_engine.io.commit_mem.idx0      := rob.io.commit_mem_idx0
        exec_engine.io.commit_mem.valid1    := rob.io.commit_mem_valid1
        exec_engine.io.commit_mem.idx1      := rob.io.commit_mem_idx1

        // ---------------- Commit 提交与系统状态 ----------------
        rob.io.has_int          := csr.io.hasInt
        rob.io.csr_tlbrentryOut := csr.io.tlbrentryOut
        rob.io.csr_eentryOut    := csr.io.eentryOut
        rob.io.csr_eraOut       := csr.io.eraOut
        rob.io.csr_tlbidx_out   := csr.io.tlbidx_out
        rob.io.csr_tlb_out      := csr.io.tlb_out

        rename.io.commit_valid := rob.io.commit_valid
        rename.io.commit_we    := rob.io.commit_we
        rename.io.commit_raddr := rob.io.commit_waddr
        rename.io.commit_paddr := rob.io.commit_paddr
        rename.io.commit_old_p := rob.io.commit_old_p

        rename.io.commit1_valid := rob.io.commit1_valid
        rename.io.commit1_we    := rob.io.commit1_we
        rename.io.commit1_raddr := rob.io.commit1_waddr
        rename.io.commit1_paddr := rob.io.commit1_paddr
        rename.io.commit1_old_p := rob.io.commit1_old_p

        // CSR 提交
        csr.io.waddr     := rob.io.commit_csr_num
        csr.io.writeEn   := rob.io.commit_csr_we
        csr.io.writeData := rob.io.commit_csr_wdata 
        csr.io.writeMask := rob.io.commit_csr_wmask
        csr.io.excValid  := rob.io.commit_has_exc
        csr.io.excEcode  := rob.io.commit_ecode
        csr.io.excEsubcode:= 0.U
        csr.io.excPc     := rob.io.commit_pc_out
        csr.io.excAddr   := rob.io.commit_exc_addr
        csr.io.ertnFlush := rob.io.commit_ertn
        
        // TLB 提交
        tlb_module.io.we      := rob.io.tlb_we
        tlb_module.io.w_index := rob.io.tlb_w_idx
        tlb_module.io.w_dat   := rob.io.tlb_w_dat
        csr.io.tlbrd_we       := rob.io.commit_tlbrd_we
        tlb_module.io.r_index := csr.io.tlbidx_out
        csr.io.tlbrd_in       := tlb_module.io.r_dat

        // ---------------- 控制流与 Flush (前端清空执行) ----------------
        


        // ---------------- 访存 Cache 完美还原与仲裁 ----------------
        val if_req_valid = if_module.io.cache_io.req
        
        // ★ 核心修复：用一个极小的 1 深度 Queue，彻底斩断 LSQ 到 ICache 的 23级长连线！
        class AguIcacheReq extends Bundle {
            val addr     = UInt(32.W)
            val req_id   = UInt(8.W)
            val cacop_op = UInt(2.W)
        }
        // flow=false, pipe=false 保证了出队和入队的 valid/ready 信号纯粹由寄存器打出，不包含任何组合逻辑回路！
        val agu_icache_q = Module(new Queue(new AguIcacheReq(), 1, pipe = false, flow = false))
        
        // --- 队列输入端 (接驳 ExecEngine/LSQ) ---
        val is_agu_to_icache = exec_engine.io.data_sram.req && exec_engine.io.cacop_is_icache
        agu_icache_q.io.enq.valid         := is_agu_to_icache
        agu_icache_q.io.enq.bits.addr     := exec_engine.io.data_sram.addr
        agu_icache_q.io.enq.bits.req_id   := exec_engine.io.lsq_req_id
        agu_icache_q.io.enq.bits.cacop_op := exec_engine.io.cacop_op
        
        // --- 队列输出端 (接驳原有仲裁逻辑) ---
        // 原来的 agu_icache_req 现在从队列的寄存器纯净输出读取！
        val agu_icache_req = agu_icache_q.io.deq.valid
        val q_addr         = agu_icache_q.io.deq.bits.addr
        val q_req_id       = agu_icache_q.io.deq.bits.req_id
        val q_cacop_op     = agu_icache_q.io.deq.bits.cacop_op

        // ==========================================
        // ★ 核心修复：基于 9位 Ticket ID 的绝对安全路由
        // ==========================================
        // 提取最高位 [8]：0 代表取指前端 (IF)，1 代表访存单元 (AGU)
        val is_if_resp  = icache.io.cpu.data_ok && (icache.io.cpu.ret_id(8) === 0.U)
        val is_agu_resp = icache.io.cpu.data_ok && (icache.io.cpu.ret_id(8) === 1.U)

        // IF 发请求条件：只要 AGU 不发，IF 就可以发 (不再需要等 Cache 变空！)
        val agu_icache_req_reg = RegNext(agu_icache_req, false.B)
        val can_issue_if  = !agu_icache_req_reg && !agu_icache_req
        val actual_if_req = if_req_valid && can_issue_if
        
        // AGU 发请求条件：IF 当拍不发，AGU 优先发
        val can_issue_agu = !actual_if_req
        val actual_agu_req = agu_icache_req && can_issue_agu

        val agu_fire = agu_icache_q.io.deq.valid && agu_icache_q.io.deq.ready
        agu_icache_q.io.deq.ready := icache.io.cpu.addr_ok && can_issue_agu

        // 3. ICache 路由 (严谨的 MUX)
        val off_bit = icfg.offsetBits - 1
        val idx_bit = icfg.offsetBits + icfg.indexBits - 1
        val tag_bit = 31

        icache.io.cpu.req_id := Mux(actual_agu_req, Cat(1.U(1.W), q_req_id), Cat(0.U(1.W), if_module.io.inst_req_id))
        icache.io.cpu.valid  := actual_if_req || actual_agu_req
        icache.io.cpu.op     := false.B
        // ★ 地址切片全部改用 q_addr，斩断远端组合逻辑！
        icache.io.cpu.index  := Mux(actual_agu_req, q_addr(idx_bit, off_bit + 1), if_module.io.cache_io.addr(idx_bit, off_bit + 1))
        icache.io.cpu.tag    := Mux(actual_agu_req, q_addr(tag_bit, idx_bit + 1), if_module.io.cache_io.addr(tag_bit, idx_bit + 1))
        icache.io.cpu.offset := Mux(actual_agu_req, q_addr(off_bit, 0), if_module.io.cache_io.addr(off_bit, 0))
        icache.io.cpu.wstrb  := 0.U
        icache.io.cpu.wdata  := 0.U
        icache.io.cpu.uncached := if_module.io.inst_uncached
        
        icache.io.cpu.cacop_en := actual_agu_req
        icache.io.cpu.cacop_op := Mux(actual_agu_req, q_cacop_op, 0.U)

        // 4. ★ 响应精准分发
        // ★ 修复 2：IF 的 addr_ok 只能看优先级(can_issue_if)，绝不能看 IF 当拍有没有发 req！
        // 4. ★ 响应精准分发
        if_module.io.cache_io.addr_ok := icache.io.cpu.addr_ok && can_issue_if
        if_module.io.cache_io.data_ok := is_if_resp   // ★ 只吃属于 IF 的数据！
        if_module.io.cache_io.rdata   := icache.io.cpu.rdata
        if_module.io.inst_ret_id       := icache.io.cpu.ret_id(7, 0) // ★ 归还 8 位取餐码

        bridge.io.inst_cache <> icache.io.axi

        // 5. DCache 路由 (★ 插入极速队列，彻底斩断跨模块布线延迟黑洞)
        class LsqDcacheReq extends Bundle {
            val req_id   = UInt(8.W)
            val op       = Bool()
            val addr     = UInt(32.W)
            val wstrb    = UInt(4.W)
            val wdata    = UInt(32.W)
            val uncached = Bool()
            val cacop_en = Bool()
            val cacop_op = UInt(2.W)
        }
        // flow=false, pipe=false 保证切断一切组合逻辑前馈，化身物理隔离墙！
        val lsq_dcache_q = Module(new Queue(new LsqDcacheReq(), 2, pipe = false, flow = false))

        // --- 队列输入端 (接驳 ExecEngine/LSQ) ---
        val is_lsq_to_dcache = exec_engine.io.data_sram.req && !exec_engine.io.cacop_is_icache
        
        lsq_dcache_q.io.enq.valid         := is_lsq_to_dcache
        lsq_dcache_q.io.enq.bits.req_id   := exec_engine.io.lsq_req_id
        lsq_dcache_q.io.enq.bits.op       := exec_engine.io.data_sram.wr
        lsq_dcache_q.io.enq.bits.addr     := exec_engine.io.data_sram.addr
        lsq_dcache_q.io.enq.bits.wstrb    := exec_engine.io.data_sram.wstrb
        lsq_dcache_q.io.enq.bits.wdata    := exec_engine.io.data_sram.wdata
        lsq_dcache_q.io.enq.bits.uncached := exec_engine.io.data_uncached
        lsq_dcache_q.io.enq.bits.cacop_en := exec_engine.io.cacop_en && !exec_engine.io.cacop_is_icache
        lsq_dcache_q.io.enq.bits.cacop_op := exec_engine.io.cacop_op

        val dq = lsq_dcache_q.io.deq

        // --- 队列输出端 (接驳 DCache) ---
        dcache.io.cpu.req_id := Cat(1.U(1.W), dq.bits.req_id)
        dcache.io.cpu.valid  := dq.valid
        dcache.io.cpu.op     := dq.bits.op
        
        // ★ 核心替换：Cache 内部切片全部改用队列弹出的稳定寄存器地址！
        dcache.io.cpu.index  := dq.bits.addr(idx_bit, off_bit + 1)
        dcache.io.cpu.tag    := dq.bits.addr(tag_bit, idx_bit + 1)
        dcache.io.cpu.offset := dq.bits.addr(off_bit, 0)
        
        dcache.io.cpu.wstrb  := dq.bits.wstrb
        dcache.io.cpu.wdata  := dq.bits.wdata
        dcache.io.cpu.uncached := dq.bits.uncached
        dcache.io.cpu.cacop_en := dq.bits.cacop_en
        dcache.io.cpu.cacop_op := dq.bits.cacop_op

        // ★ 出队握手：当 DCache 准备好接收时，通知队列弹出
        dq.ready := dcache.io.cpu.addr_ok

        bridge.io.data_cache <> dcache.io.axi

        // =====================================================================
        // ★ 终极修复：请求与响应通道彻底解耦！
        // =====================================================================
        // 1. 请求通道 (addr_ok)
        // ★ 时序起飞：LSQ 无论是去 ICache 还是 DCache，看到的 addr_ok 统统是极近的队列 ready 信号！
        exec_engine.io.data_sram.addr_ok := Mux(exec_engine.io.cacop_is_icache, agu_icache_q.io.enq.ready, lsq_dcache_q.io.enq.ready)
        
        // 2. 响应通道 (data_ok/rdata/ret_id) 绝对不能用 cacop_is_icache 选！
        // 因为数据异步返回时，前端可能在发呆，cacop_is_icache 会读到前世的幽灵垃圾！
        // 必须通过 ICache 自己是否在 Pending 来做独立判定！
        // ★ 修复 2：彻底解耦返回通道！让数据自己说话！
        // ★ 修复 2：彻底解耦返回通道！让数据自己说话！
        val is_icache_resp_final = is_agu_resp // 使用上面算好的、绝对纯净的 AGU 响应标志
        val is_dcache_resp = dcache.io.cpu.data_ok

        // 异步回来的数据，只认自己的 data_ok，跟指令状态彻底脱钩！
        exec_engine.io.data_sram.data_ok := is_dcache_resp || is_icache_resp_final
        // ★ 核心隔离：如果大家都没响应，就保持纯净的 0，绝不让 ICache 的杂音脏了 LSQ 的波形！
        exec_engine.io.data_sram.rdata   := Mux(is_dcache_resp, dcache.io.cpu.rdata, 
                                            Mux(is_icache_resp_final, icache.io.cpu.rdata, 0.U))
        
        exec_engine.io.lsq_ret_id        := Mux(is_dcache_resp, dcache.io.cpu.ret_id(7, 0), icache.io.cpu.ret_id(7, 0))


        // ---------------- AXI 与 Debug 连线 ----------------
        arid    := bridge.io.axi.arid
        araddr  := bridge.io.axi.araddr
        arlen   := bridge.io.axi.arlen
        arsize  := bridge.io.axi.arsize
        arburst := bridge.io.axi.arburst
        arlock  := bridge.io.axi.arlock
        arcache := bridge.io.axi.arcache
        arprot  := bridge.io.axi.arprot
        arvalid := bridge.io.axi.arvalid
        bridge.io.axi.arready := arready

        bridge.io.axi.rid     := rid
        bridge.io.axi.rdata   := rdata
        bridge.io.axi.rresp   := rresp
        bridge.io.axi.rlast   := rlast
        bridge.io.axi.rvalid  := rvalid
        rready  := bridge.io.axi.rready

        awid    := bridge.io.axi.awid
        awaddr  := bridge.io.axi.awaddr
        awlen   := bridge.io.axi.awlen
        awsize  := bridge.io.axi.awsize
        awburst := bridge.io.axi.awburst
        awlock  := bridge.io.axi.awlock
        awcache := bridge.io.axi.awcache
        awprot  := bridge.io.axi.awprot
        awvalid := bridge.io.axi.awvalid
        bridge.io.axi.awready := awready

        // =====================================================================
        // ★ 终极防线：全通道 AXI Skid Buffer (Register Slice)
        // 彻底斩断 CPU 与外部 SoC 的所有组合逻辑路径，保住时钟频率，杜绝死锁！
        // =====================================================================

        // --- 1. 定义 5 个标准 AXI 通道的 Bundle ---
        class AxiArChannel extends Bundle {
            val id    = UInt(4.W)
            val addr  = UInt(32.W)
            val len   = UInt(8.W)
            val size  = UInt(3.W)
            val burst = UInt(2.W)
            val lock  = UInt(2.W)
            val cache = UInt(4.W)
            val prot  = UInt(3.W)
        }
        class AxiRChannel extends Bundle {
            val id    = UInt(4.W)
            val data  = UInt(32.W)
            val resp  = UInt(2.W)
            val last  = Bool()
        }
        class AxiAwChannel extends Bundle {
            val id    = UInt(4.W)
            val addr  = UInt(32.W)
            val len   = UInt(8.W)
            val size  = UInt(3.W)
            val burst = UInt(2.W)
            val lock  = UInt(2.W)
            val cache = UInt(4.W)
            val prot  = UInt(3.W)
        }
        class AxiWChannel extends Bundle {
            val id    = UInt(4.W)
            val data  = UInt(32.W)
            val strb  = UInt(4.W)
            val last  = Bool()
        }
        class AxiBChannel extends Bundle {
            val id    = UInt(4.W)
            val resp  = UInt(2.W)
        }

        // --- 2. 实例化深度为 2 的队列 (标准 Skid Buffer 深度) ---
        val ar_q = Module(new Queue(new AxiArChannel(), 2))
        val r_q  = Module(new Queue(new AxiRChannel(), 2))
        val aw_q = Module(new Queue(new AxiAwChannel(), 2))
        val w_q  = Module(new Queue(new AxiWChannel(), 2))
        val b_q  = Module(new Queue(new AxiBChannel(), 2))

        // --- 3. AR 通道 (Master -> Slave) ---
        ar_q.io.enq.valid      := bridge.io.axi.arvalid
        bridge.io.axi.arready  := ar_q.io.enq.ready
        ar_q.io.enq.bits.id    := bridge.io.axi.arid
        ar_q.io.enq.bits.addr  := bridge.io.axi.araddr
        ar_q.io.enq.bits.len   := bridge.io.axi.arlen
        ar_q.io.enq.bits.size  := bridge.io.axi.arsize
        ar_q.io.enq.bits.burst := bridge.io.axi.arburst
        ar_q.io.enq.bits.lock  := bridge.io.axi.arlock
        ar_q.io.enq.bits.cache := bridge.io.axi.arcache
        ar_q.io.enq.bits.prot  := bridge.io.axi.arprot

        arvalid := ar_q.io.deq.valid
        ar_q.io.deq.ready := arready
        arid    := ar_q.io.deq.bits.id
        araddr  := ar_q.io.deq.bits.addr
        arlen   := ar_q.io.deq.bits.len
        arsize  := ar_q.io.deq.bits.size
        arburst := ar_q.io.deq.bits.burst
        arlock  := ar_q.io.deq.bits.lock
        arcache := ar_q.io.deq.bits.cache
        arprot  := ar_q.io.deq.bits.prot

        // --- 4. R 通道 (Slave -> Master) 注意方向相反！ ---
        r_q.io.enq.valid     := rvalid
        rready               := r_q.io.enq.ready
        r_q.io.enq.bits.id   := rid
        r_q.io.enq.bits.data := rdata
        r_q.io.enq.bits.resp := rresp
        r_q.io.enq.bits.last := rlast

        bridge.io.axi.rvalid := r_q.io.deq.valid
        r_q.io.deq.ready     := bridge.io.axi.rready
        bridge.io.axi.rid    := r_q.io.deq.bits.id
        bridge.io.axi.rdata  := r_q.io.deq.bits.data
        bridge.io.axi.rresp  := r_q.io.deq.bits.resp
        bridge.io.axi.rlast  := r_q.io.deq.bits.last

        // --- 5. AW 通道 (Master -> Slave) ---
        aw_q.io.enq.valid      := bridge.io.axi.awvalid
        bridge.io.axi.awready  := aw_q.io.enq.ready
        aw_q.io.enq.bits.id    := bridge.io.axi.awid
        aw_q.io.enq.bits.addr  := bridge.io.axi.awaddr
        aw_q.io.enq.bits.len   := bridge.io.axi.awlen
        aw_q.io.enq.bits.size  := bridge.io.axi.awsize
        aw_q.io.enq.bits.burst := bridge.io.axi.awburst
        aw_q.io.enq.bits.lock  := bridge.io.axi.awlock
        aw_q.io.enq.bits.cache := bridge.io.axi.awcache
        aw_q.io.enq.bits.prot  := bridge.io.axi.awprot

        awvalid := aw_q.io.deq.valid
        aw_q.io.deq.ready := awready
        awid    := aw_q.io.deq.bits.id
        awaddr  := aw_q.io.deq.bits.addr
        awlen   := aw_q.io.deq.bits.len
        awsize  := aw_q.io.deq.bits.size
        awburst := aw_q.io.deq.bits.burst
        awlock  := aw_q.io.deq.bits.lock
        awcache := aw_q.io.deq.bits.cache
        awprot  := aw_q.io.deq.bits.prot

        // --- 6. W 通道 (Master -> Slave) ---
        w_q.io.enq.valid     := bridge.io.axi.wvalid
        bridge.io.axi.wready := w_q.io.enq.ready
        w_q.io.enq.bits.id   := bridge.io.axi.wid
        w_q.io.enq.bits.data := bridge.io.axi.wdata
        w_q.io.enq.bits.strb := bridge.io.axi.wstrb
        w_q.io.enq.bits.last := bridge.io.axi.wlast

        wvalid := w_q.io.deq.valid
        w_q.io.deq.ready := wready
        wid    := w_q.io.deq.bits.id
        wdata  := w_q.io.deq.bits.data
        wstrb  := w_q.io.deq.bits.strb
        wlast  := w_q.io.deq.bits.last

        // --- 7. B 通道 (Slave -> Master) 注意方向相反！ ---
        b_q.io.enq.valid     := bvalid
        bready               := b_q.io.enq.ready
        b_q.io.enq.bits.id   := bid
        b_q.io.enq.bits.resp := bresp

        bridge.io.axi.bvalid := b_q.io.deq.valid
        b_q.io.deq.ready     := bridge.io.axi.bready
        bridge.io.axi.bid    := b_q.io.deq.bits.id
        bridge.io.axi.bresp  := b_q.io.deq.bits.resp
        
        // --- Debug 端口连线保持不变 ---
        val actual_we0 = Mux(!rob.io.commit_valid || rob.io.commit_waddr === 0.U, 0.U(4.W), Fill(4, rob.io.commit_we))
        debug0_wb_pc       := rob.io.commit_pc_out
        debug0_wb_rf_wen   := actual_we0
        debug0_wb_rf_wnum  := rob.io.commit_waddr
        debug0_wb_rf_wdata := rob.io.commit_wdata

        val actual_we1 = Mux(!rob.io.commit1_valid || rob.io.commit1_waddr === 0.U, 0.U(4.W), Fill(4, rob.io.commit1_we))
        debug1_wb_pc       := rob.io.commit1_pc
        debug1_wb_rf_wen   := actual_we1
        debug1_wb_rf_wnum  := rob.io.commit1_waddr
        debug1_wb_rf_wdata := rob.io.commit1_wdata

        ws_valid := false.B
        rf_rdata := 0.U


        probe_cdb0_pc := exec_engine.io.debug_cdb0_pc
        probe_cdb1_pc := exec_engine.io.debug_cdb1_pc

        debug0_wb_valid := rob.io.commit_valid
        debug1_wb_valid := rob.io.commit1_valid
    }
}

class LsqAllocReq extends Bundle {
    val req_type = UInt(2.W) // 0: Load, 1: Store, 2: CACOP
    val rob      = UInt(Config.robPtrWidth.W)
    val pc       = UInt(32.W)
    val pdest    = UInt(Config.prfPtrWidth.W)
    val mask     = UInt(4.W)
    val cacop    = UInt(5.W)
    val lsOp     = UInt(8.W)
}

class LsqAllocIO extends Bundle {
    val req = Flipped(Decoupled(new LsqAllocReq()))
    val idx = Output(UInt(4.W)) 
}

class LsqStatePort extends Bundle {
    val current_tail = Output(UInt(4.W))
    val br_restore   = Input(UInt(4.W))
}

class LsqViolationPort extends Bundle {
    val valid = Output(Bool())
    val rob   = Output(UInt(Config.robPtrWidth.W))
    val pc    = Output(UInt(32.W))
}

class CommitMemPort extends Bundle {
    val valid0 = Output(Bool())
    val idx0   = Output(UInt(Config.robPtrWidth.W))
    val valid1 = Output(Bool())
    val idx1   = Output(UInt(Config.robPtrWidth.W))
}
error id: file://<WORKSPACE>/src/main/scala/ExecutionEngine.scala:local29
file://<WORKSPACE>/src/main/scala/ExecutionEngine.scala
empty definition using pc, found symbol in pc: 
found definition using semanticdb; symbol local29
empty definition using fallback
non-local guesses:

offset: 2641
uri: file://<WORKSPACE>/src/main/scala/ExecutionEngine.scala
text:
```scala
package mycpu

import chisel3._
import chisel3.util._

class CdbArbiter extends Module {
    val io = IO(new Bundle {
        // ★ 扩容为 5 个输入请求 (0: MDU, 1: LSQ, 2: AGU, 3: ALU0, 4: ALU1)
        val reqs = Vec(5, Flipped(Decoupled(new PipelineData())))
        
        val cdb0 = Valid(new PipelineData())
        val cdb1 = Valid(new PipelineData())
    })

    // 把 5 个 valid 拼接成一个 5 bit 向量
    val req_valids = Cat(io.reqs(4).valid, io.reqs(3).valid, io.reqs(2).valid, io.reqs(1).valid, io.reqs(0).valid)
    val has_any_req = req_valids.orR

    val sel0 = PriorityEncoder(req_valids)
    val grant0 = Mux(has_any_req, UIntToOH(sel0), 0.U(5.W))

    val req_valids1 = req_valids & ~grant0 
    val has_any_req1 = req_valids1.orR
    val sel1 = PriorityEncoder(req_valids1)
    val grant1 = Mux(has_any_req1, UIntToOH(sel1), 0.U(5.W))

    for (i <- 0 until 5) {
        io.reqs(i).ready := grant0(i) || grant1(i)
    }

    io.cdb0.valid := has_any_req
    io.cdb0.bits  := Mux1H(grant0, io.reqs.map(_.bits))

    io.cdb1.valid := has_any_req1
    io.cdb1.bits  := Mux1H(grant1, io.reqs.map(_.bits))
}


class ExecutionEngine extends Module {
    val io = IO(new Bundle {
        // ---------------- 1. 独立发射通道 (来自 IQ) ----------------
        val in_alu0 = Flipped(Decoupled(new PipelineData()))
        val in_alu1 = Flipped(Decoupled(new PipelineData()))
        val in_mdu  = Flipped(Decoupled(new PipelineData()))
        val in_agu  = Flipped(Decoupled(new PipelineData()))

        // ---------------- 2. 双发射 CDB 总线 (去往 PRF, IQ, ROB) ----------------
        val cdb0 = Valid(new PipelineData())
        val cdb1 = Valid(new PipelineData())

        // ---------------- 3. 分支与全局控制 (去往 Front-end) ----------------
        val flush = Input(Bool())
        val br_resolve = Output(new BranchResolve())
        val branch_req = Output(Bool())
        val branch_pc  = Output(UInt(32.W))
        val timer_in   = Input(UInt(64.W))

        // ---------------- 4. 访存与 TLB 透传接口 (来自 AGU) ----------------
        val lsq_req_id = Output(UInt(8.W))
        val lsq_ret_id = Input(UInt(8.W))


        val data_sram     = new SramIo()
        val data_uncached = Output(Bool())
        val mmu_config    = Input(new MmuConfig())
        
        val tlb_s1_vppn     = Output(UInt(19.W))
        val tlb_s1_va_bit12 = Output(Bool())
        val tlb_s1_asid     = Output(UInt(10.W))
        val tlb_s1_found    = Input(Bool())
        val tlb_s1_index    = Input(UInt(4.W))  
        val tlb_s1_ppn      = Input(UInt(20.W))
        val tlb_s1_ps       = Input(UInt(6.W))
        val tlb_s1_plv      = Input(UInt(2.W))  
        val tlb_s1_mat@@      = Input(UInt(2.W))  
        val tlb_s1_d        = Input(Bool())    
        val tlb_s1_v        = Input(Bool())    
        val invtlb_valid    = Output(Bool())
        val invtlb_op       = Output(UInt(5.W))

        // ---------------- 5. LSQ 透传接口 ----------------
        val lsq_current_tail = Output(UInt(4.W)) 
        val lsq_br_restore   = Input(UInt(4.W))  
        val lsq_alloc_valid  = Input(Bool())
        val lsq_alloc_type   = Input(UInt(2.W))
        val lsq_alloc_rob    = Input(UInt(Config.robPtrWidth.W))
        val lsq_alloc_pc     = Input(UInt(32.W))
        val lsq_alloc_pdest  = Input(UInt(6.W))
        val lsq_alloc_mask   = Input(UInt(4.W))
        val lsq_alloc_cacop  = Input(UInt(5.W))
        val lsq_alloc_ready  = Output(Bool())
        val lsq_alloc_idx    = Output(UInt(4.W))
        val lsq_violation_valid = Output(Bool())
        val lsq_violation_rob   = Output(UInt(Config.robPtrWidth.W))
        val lsq_violation_pc    = Output(UInt(32.W))
        val commit_mem_valid0 = Input(Bool())
        val commit_mem_idx0   = Input(UInt(Config.robPtrWidth.W))
        val commit_mem_valid1 = Input(Bool())
        val commit_mem_idx1   = Input(UInt(Config.robPtrWidth.W))


        val csr_raddr = Output(UInt(14.W))
        val csr_rdata = Input(UInt(32.W))

        val cacop_en = Output(Bool())
        val cacop_op = Output(UInt(2.W))
        val cacop_is_icache = Output(Bool())

        val lsq_alloc_lsOp   = Input(UInt(8.W))

        val bpu_update = Valid(new BpuUpdate())

        // ★ 强制防优化的 Debug 端口
        val debug_cdb0_pc = Output(UInt(32.W))
        val debug_cdb1_pc = Output(UInt(32.W))
    })

    // ==========================================
    // 实例化四大天王与仲裁器
    // ==========================================
    val alu0 = Module(new AluUnit())       // 全能 (处理分支)
    val alu1 = Module(new AluSimpleUnit()) // 打手 (纯算术)
    val mdu  = Module(new MduUnit())       // 算筹 (乘除法)
    val agu  = Module(new AguUnit())       // 镖局 (访存与TLB)
    
    val arbiter = Module(new CdbArbiter())
    val lsq = Module(new LSQ())

    // ==========================================
    // 输入通道接驳
    // ==========================================
    alu0.io.in <> io.in_alu0
    alu1.io.in <> io.in_alu1
    mdu.io.in  <> io.in_mdu
    agu.io.in  <> io.in_agu

    alu0.io.flush := io.flush
    alu1.io.flush := io.flush
    mdu.io.flush  := io.flush
    agu.io.flush  := io.flush

    // ==========================================
    // 广播网接驳 (ALU0 的分支结果通报全军)
    // ==========================================
    val global_br_resolve = alu0.io.br_resolve
    
    alu0.io.br_resolve_in := global_br_resolve
    alu1.io.br_resolve_in := global_br_resolve
    mdu.io.br_resolve_in  := global_br_resolve
    agu.io.br_resolve_in  := global_br_resolve
    
    io.br_resolve := global_br_resolve
    io.branch_req := alu0.io.branch_req
    io.branch_pc  := alu0.io.branch_pc
    io.bpu_update := alu0.io.bpu_update  // ★ 接这根线出来

    alu0.io.timer_in := io.timer_in

    // ==========================================
    // 访存与 TLB 专线 (AGU 独占)
    // ==========================================
    agu.io.mmu_config := io.mmu_config

    //原来这俩都是接哪儿的？
    io.data_sram <> agu.io.data_sram
    io.data_uncached := agu.io.data_uncached


    //这部分不能一键连接吗？一定要这么多行吗？能不能想办法？
    // 优雅解包 TlbSearchPort
    io.tlb_s1_vppn       := agu.io.tlb_port.vppn
    io.tlb_s1_va_bit12   := agu.io.tlb_port.va_bit12
    io.tlb_s1_asid       := agu.io.tlb_port.asid
    
    // 优雅打包 TlbSearchPort
    agu.io.tlb_port.found:= io.tlb_s1_found
    agu.io.tlb_port.index:= io.tlb_s1_index
    agu.io.tlb_port.ppn  := io.tlb_s1_ppn
    agu.io.tlb_port.ps   := io.tlb_s1_ps
    agu.io.tlb_port.plv  := io.tlb_s1_plv
    agu.io.tlb_port.mat  := io.tlb_s1_mat
    agu.io.tlb_port.d    := io.tlb_s1_d
    agu.io.tlb_port.v    := io.tlb_s1_v

    io.invtlb_valid    := agu.io.invtlb_valid
    io.invtlb_op       := agu.io.invtlb_op



    // ==========================================
    // 仲裁器接驳 (扩容为 5 车道)
    // ==========================================
    arbiter.io.reqs(0) <> mdu.io.out
    arbiter.io.reqs(1) <> lsq.io.lsq_wb    // LSQ 抢占 1 号道 (Load 数据回写)
    // ★ AGU 王者归来 2 号道！(广播 Store/异常)
    // ★ 终极拦截：AGU 的“静音面罩”
    // Load 哪怕发生异常，AGU 也保持静音，扔给 LSQ，由 LSQ 负责写回 CDB 报错！
    val agu_needs_cdb = agu.io.out.bits.memWe || agu.io.out.bits.is_cacop || (agu.io.out.bits.tlbOp =/= 0.U)

    // =====================================================================
    // ★ 核心时序切割：给 AGU 单独配一个写回队列，斩断 TLB -> Arbiter -> ROB 的 22 级死亡连线！
    // 深度设置为 2，保证背靠背 Store 指令的满血吞吐率，同时构筑绝对的物理隔离墙！
    // =====================================================================
    val agu_cdb_q = withReset(reset.asBool || io.flush) { Module(new Queue(new PipelineData(), 2)) }
    
    agu_cdb_q.io.enq.valid := agu.io.out.valid && agu_needs_cdb
    agu_cdb_q.io.enq.bits  := agu.io.out.bits
    
    arbiter.io.reqs(2) <> agu_cdb_q.io.deq
    
    // AGU 的 ready 信号处理：如果是需要写回 CDB 的，看队列是否能收；否则直接无脑 true (扔给 LSQ 就跑)
    agu.io.out.ready := Mux(agu_needs_cdb, agu_cdb_q.io.enq.ready, true.B)
    arbiter.io.reqs(3) <> alu0.io.out
    arbiter.io.reqs(4) <> alu1.io.out

    // 输出最终的 CDB！
    io.cdb0 := arbiter.io.cdb0
    io.cdb1 := arbiter.io.cdb1

    io.csr_raddr := alu0.io.csr_raddr
    alu0.io.csr_rdata := io.csr_rdata
    

    // ==========================================
    // 终极连线：AGU -> LSQ -> Top
    // ==========================================
    //这几部分同理，你不能改改lsq他们的接口，让他可以一键连过来吗？
    lsq.io.flush      := io.flush
    lsq.io.br_resolve := global_br_resolve

    io.lsq_req_id := lsq.io.dcache_req_id
    lsq.io.dcache_ret_id := io.lsq_ret_id

    lsq.io.alloc_valid := io.lsq_alloc_valid
    lsq.io.alloc_type  := io.lsq_alloc_type
    lsq.io.alloc_rob   := io.lsq_alloc_rob
    lsq.io.alloc_pc    := io.lsq_alloc_pc
    lsq.io.alloc_pdest := io.lsq_alloc_pdest
    lsq.io.alloc_mask  := io.lsq_alloc_mask
    lsq.io.alloc_cacop := io.lsq_alloc_cacop
    io.lsq_alloc_ready := lsq.io.alloc_ready
    io.lsq_alloc_idx   := lsq.io.alloc_idx
    io.lsq_current_tail:= lsq.io.current_lsq_tail
    lsq.io.br_restore_tail := io.lsq_br_restore

    lsq.io.agu_in_valid   := agu.io.to_lsq.valid && agu.io.out.ready

    lsq.io.agu_in_lsqIdx  := agu.io.out.bits.lsq_idx
    // 彻底让 CDB 广播网 和 访存物理网 解耦！数据直接从 Agu2Lsq 包裹里拿！
    lsq.io.agu_in_lsqIdx  := agu.io.to_lsq.bits.lsqIdx
    lsq.io.agu_in_paddr   := agu.io.to_lsq.bits.paddr
    lsq.io.agu_in_size    := agu.io.to_lsq.bits.size
    lsq.io.agu_in_uncached:= agu.io.to_lsq.bits.uncached
    lsq.io.agu_in_wdata   := agu.io.to_lsq.bits.wdata
    lsq.io.agu_in_wstrb   := agu.io.to_lsq.bits.wstrb
    lsq.io.agu_in_exc     := agu.io.to_lsq.bits.has_exc
    lsq.io.agu_in_ecode   := agu.io.to_lsq.bits.ecode

    // Cache 透传完全交给 LSQ 控制 (AGU 成功隐退)
    io.data_sram       <> lsq.io.dcache
    io.data_uncached   := lsq.io.dcache_uncached
    io.cacop_en        := lsq.io.cacop_en
    io.cacop_op        := lsq.io.cacop_op
    io.cacop_is_icache := lsq.io.cacop_is_icache

    io.lsq_violation_valid := lsq.io.lsq_violation_valid
    io.lsq_violation_rob   := lsq.io.lsq_violation_rob
    io.lsq_violation_pc    := lsq.io.lsq_violation_pc

    lsq.io.commit_mem_valid0 := io.commit_mem_valid0
    lsq.io.commit_mem_idx0   := io.commit_mem_idx0
    lsq.io.commit_mem_valid1 := io.commit_mem_valid1
    lsq.io.commit_mem_idx1   := io.commit_mem_idx1

    lsq.io.alloc_lsOp := io.lsq_alloc_lsOp


    ///////////
    io.debug_cdb0_pc := arbiter.io.cdb0.bits.pc
    io.debug_cdb1_pc := arbiter.io.cdb1.bits.pc
}
```


#### Short summary: 

empty definition using pc, found symbol in pc: 
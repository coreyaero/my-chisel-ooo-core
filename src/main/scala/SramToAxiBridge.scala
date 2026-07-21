package mycpu

import chisel3._
import chisel3.util._

class SramToAxiBridge(implicit p: CacheConfig) extends Module {
    val io = IO(new Bundle {
        val inst_cache = Flipped(new CacheToAxiIO())
        val data_cache = Flipped(new CacheToAxiIO())
        val axi = new AxiIO()
    })

    val inst_req_read  = io.inst_cache.rd_req
    val data_req_read  = io.data_cache.rd_req
    val data_req_write = io.data_cache.wr_req

    // =========================================================================
    // ★ 引擎 0：MMIO 强序微锁 (Uncached 飞行追踪)
    // =========================================================================
    val w_idle :: w_wait_all :: w_wait_aw :: w_wait_w :: Nil = Enum(4)
    val w_state = RegInit(w_idle)

    val uc_w_pending = RegInit(false.B)
    val is_uncached_write_req = data_req_write && (io.data_cache.wr_type === 2.U)
    // 微锁判定：如果正在受理一个 Uncached Write，且当前已经有 Uncached Write 在飞行，拦截！
    // 如果是 16 字节 Cache 行写回 (wr_type == 4.U)，绝不拦截！
    // ★ 将安全判定独立为 Wire，替代直接读取 Output，彻底杜绝组合逻辑环
    val w_safe_to_fire = !(is_uncached_write_req && uc_w_pending)

    // 当写状态机受理了一个非缓存写时，挂起微锁警戒牌
    when(w_state === w_idle && is_uncached_write_req && w_safe_to_fire) {
        uc_w_pending := true.B
    } .elsewhen(io.axi.bvalid && io.axi.bready) {
        // 收到 AXI B 通道的写完成确认，解除警戒
        uc_w_pending := false.B
    }

    // =========================================================================
    // 引擎 1：读请求通道 (AR) - 精准读拦截与智能仲裁
    // =========================================================================
    val ar_idle :: ar_wait_ready :: Nil = Enum(2)
    val ar_state = RegInit(ar_idle)

    val ar_grant_id = RegInit(0.U(4.W))
    val ar_addr_reg = RegInit(0.U(32.W))
    val ar_size_reg = RegInit(0.U(3.W))

    // ★ 读安全许可拆分：DCache 和 ICache 各论各的
    val dcache_read_safe = data_req_read && !(io.data_cache.rd_type === 2.U && uc_w_pending)
    val icache_read_safe = inst_req_read // ICache 不碰 MMIO，永远安全

    // 读请求开火：通道空闲，且至少有一个安全合法的请求
    val ar_fire = (ar_state === ar_idle) && (dcache_read_safe || icache_read_safe)

    when(ar_fire) {
        ar_state := ar_wait_ready
        // 仲裁：数据访存优先，前提是它的请求没被 MMIO 微锁拦截
        when(dcache_read_safe) {
            ar_grant_id := Cat(1.U(1.W), io.data_cache.rd_id(2, 0))
            ar_addr_reg := io.data_cache.rd_addr
            ar_size_reg := io.data_cache.rd_type
        } .otherwise {
            ar_grant_id := Cat(0.U(1.W), io.inst_cache.rd_id(2, 0))
            ar_addr_reg := io.inst_cache.rd_addr
            ar_size_reg := io.inst_cache.rd_type 
        }
    } .elsewhen(ar_state === ar_wait_ready && io.axi.arready) {
        ar_state := ar_idle
    }

    io.axi.arvalid := (ar_state === ar_wait_ready) 
    io.axi.arid    := ar_grant_id
    io.axi.araddr  := ar_addr_reg
    // 4.U 代表 16 字节缓存行(Burst)，需要 4 拍 (arlen=3)。否则单拍 (arlen=0)
    // ★ 核心修改：读突发长度跟随 CacheConfig 动态变化
    io.axi.arlen   := Mux(ar_size_reg === 4.U, (p.lineWords - 1).U, 0.U)
    io.axi.arsize  := 2.U // 每拍固定 4 字节 (3b'010)
    io.axi.arburst := "b01".U  // INCR 模式
    io.axi.arlock  := 0.U      
    io.axi.arcache := 0.U      
    io.axi.arprot  := 0.U      

    // =========================================================================
    // 引擎 2：读响应通道 (R) - 动态 ID 路由
    // =========================================================================
    io.axi.rready := true.B // CPU 端接收缓冲永远就绪

    // 数据端 (DCache) 响应：最高位为 1
    val is_data_ret = io.axi.rvalid && (io.axi.rid(3) === 1.U)
    io.data_cache.ret_valid := is_data_ret
    io.data_cache.ret_id    := io.axi.rid(2, 0) // 还给 DCache 的 MSHR ID
    io.data_cache.ret_last  := io.axi.rlast
    io.data_cache.ret_data  := io.axi.rdata

    // 指令端 (ICache) 响应：最高位为 0
    val is_inst_ret = io.axi.rvalid && (io.axi.rid(3) === 0.U)
    io.inst_cache.ret_valid := is_inst_ret
    io.inst_cache.ret_id    := io.axi.rid(2, 0)
    io.inst_cache.ret_last  := io.axi.rlast
    io.inst_cache.ret_data  := io.axi.rdata

    // =========================================================================
    // 引擎 3：写请求 (AW) 与 写数据 (W) 通道 (动态扩容)
    // =========================================================================
    val aw_addr_reg = RegInit(0.U(32.W))
    val aw_size_reg = RegInit(0.U(3.W))
    
    // ★ 核心修改：动态节拍计数器，支持任意长度的 Burst
    val w_beat_cnt  = RegInit(0.U(log2Ceil(p.lineWords).W)) 
    // ★ 核心修改：写数据缓冲拓宽至适应 32B (256 bits)
    val w_data_reg  = RegInit(0.U((p.lineWords * 32).W)) 
    val w_strb_reg  = RegInit(0.U(4.W))

    val aw_fire  = io.axi.awready && io.axi.awvalid
    val w_fire   = io.axi.wready && io.axi.wvalid
    val w_finish = w_fire && io.axi.wlast

    

    when(w_state === w_idle) {
        // 只有当安全时才受理写请求
        when(data_req_write && w_safe_to_fire) { 
            w_state     := w_wait_all
            aw_addr_reg := io.data_cache.wr_addr
            aw_size_reg := io.data_cache.wr_type
            w_data_reg  := io.data_cache.wr_data
            w_strb_reg  := io.data_cache.wr_wstrb
            w_beat_cnt  := 0.U
        }
    } .elsewhen(w_state === w_wait_all) {
        when(aw_fire && w_finish) { w_state := w_idle }
        .elsewhen(aw_fire)        { w_state := w_wait_w }
        .elsewhen(w_finish)       { w_state := w_wait_aw }
    } .elsewhen(w_state === w_wait_aw) {
        when(aw_fire) { w_state := w_idle }
    } .elsewhen(w_state === w_wait_w) {
        when(w_finish) { w_state := w_idle }
    }

    when(w_fire && !io.axi.wlast) {
        w_beat_cnt := w_beat_cnt + 1.U
    }

    val is_burst_write = (aw_size_reg === 4.U)

    io.axi.awvalid := (w_state === w_wait_all) || (w_state === w_wait_aw)
    io.axi.awid    := 1.U      // 写通道 ID 固定
    io.axi.awaddr  := aw_addr_reg
    io.axi.awsize  := 2.U
    // ★ 核心修改：写突发长度跟随 CacheConfig 动态变化
    io.axi.awlen   := Mux(is_burst_write, (p.lineWords - 1).U, 0.U)
    io.axi.awburst := "b01".U  
    io.axi.awlock  := 0.U      
    io.axi.awcache := 0.U      
    io.axi.awprot  := 0.U      
    
    io.axi.wvalid  := (w_state === w_wait_all) || (w_state === w_wait_w)
    io.axi.wid     := 1.U      
    val w_data_vec = Wire(Vec(p.lineWords, UInt(32.W)))
    for (i <- 0 until p.lineWords) {
        w_data_vec(i) := w_data_reg(i*32+31, i*32)
    }
    
    // 无论是单次写(取第0个)还是突发写，直接用计数器索引
    io.axi.wdata   := w_data_vec(w_beat_cnt)
    
    // 突发传输全掩码，单字传输用原始掩码
    io.axi.wstrb   := Mux(is_burst_write, "hf".U, w_strb_reg)
    
    // ★ 核心修改：最后一拍的判定信号
    io.axi.wlast   := Mux(is_burst_write, w_beat_cnt === (p.lineWords - 1).U, true.B)

    // =========================================================================
    // 引擎 4：写响应通道 (B)
    // =========================================================================
    io.axi.bready := true.B // 永远准备好接收写确认，不需要阻塞任何东西
    // 我们可以直接吞掉 bvalid，因为架构无需等它来解除锁定

    // =========================================================================
    // 终极反馈：释放 Cache 发射端
    // =========================================================================
    // ICache 只需要避让“合法且安全”的 DCache 请求
    io.inst_cache.rd_rdy := (ar_state === ar_idle) && icache_read_safe && !dcache_read_safe
    io.inst_cache.wr_rdy := true.B 

    // DCache 读写反馈严格绑定各自的安全锁
    io.data_cache.rd_rdy := (ar_state === ar_idle) && dcache_read_safe
    io.data_cache.wr_rdy := (w_state === w_idle) && w_safe_to_fire
}

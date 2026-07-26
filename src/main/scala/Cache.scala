package mycpu

import chisel3._
import chisel3.util._
import chisel3.util.experimental.loadMemoryFromFileInline
import firrtl.annotations.MemoryLoadFileType

// =========================================================================
// 全局配置类：控制 Cache 的所有规格
// =========================================================================
case class CacheConfig(
    ways: Int = 4,         // 4 路组相联
    lineWords: Int = 8,    // 行大小：8个字 (32 Bytes)
    sets: Int = 256        // 256 组 (4 * 32 * 256 = 32KB)
) {
    val lineBytes  = lineWords * 4
    val offsetBits = log2Ceil(lineBytes)   // 32B -> 5 bits (4:0)
    val indexBits  = log2Ceil(sets)        // 256 -> 8 bits (12:5)
    val tagBits    = 32 - offsetBits - indexBits // 32 - 5 - 8 = 19 bits
    val wayBits    = if (ways == 1) 1 else log2Ceil(ways)
}

// =========================================================================
// 第一步：构建绝对安全的 1R1W (一读一写) 两路组相联 Cache 物理阵列
// 特性 1：读写端口物理分离，支持同拍查表与回填。
// 特性 2：包含 Way0 和 Way1 完整两路数据。
// 特性 3：强制初始化 TagV，防止上电产生幽灵 Hit。
// 特性 4：两路独立的 Read-Under-Write 旁路前递，彻底杜绝 X 态。
// =========================================================================
// =========================================================================
// 终极重构版：绝对安全的 1R1W 参数化物理阵列
// =========================================================================
class CacheArray1R1W(implicit p: CacheConfig) extends Module {
    val io = IO(new Bundle {
        // --- 读端口 (Read Port) ---
        val r_en    = Input(Bool())
        val r_index = Input(UInt(p.indexBits.W))
        
        val r_valid = Output(Vec(p.ways, Bool()))
        val r_tag   = Output(Vec(p.ways, UInt(p.tagBits.W)))
        val r_data  = Output(Vec(p.ways, Vec(p.lineWords, UInt(32.W))))

        // --- 写端口 (Write Port) ---
        val w_way        = Input(UInt(p.wayBits.W)) 
        
        // Valid 与 Tag 分离写通道
        val w_valid_en   = Input(Bool())
        val w_valid_data = Input(Bool()) 
        val w_tag_en     = Input(Bool())
        val w_index_tag  = Input(UInt(p.indexBits.W))
        val w_tag        = Input(UInt(p.tagBits.W))
        
        // Data 写通道
        val w_data_en    = Input(Bool())
        val w_index_data = Input(UInt(p.indexBits.W))
        val w_data       = Input(Vec(p.lineWords, UInt(32.W)))
        val w_strb       = Input(Vec(p.lineWords, UInt(4.W)))
    })

    // 1. Valid 阵列 (触发器，上板绝对安全)
    val valid_array = RegInit(VecInit(Seq.fill(p.ways)(VecInit(Seq.fill(p.sets)(false.B)))))
    // 2. Tag 阵列 (BRAM)
    val tag_array = Seq.fill(p.ways)(SyncReadMem(p.sets, UInt(p.tagBits.W)))
    // 3. Data 阵列 (BRAM)
    val data_array = Seq.fill(p.ways)(Seq.fill(p.lineWords)(SyncReadMem(p.sets, Vec(4, UInt(8.W)))))

    // ==========================================
    // 冲突侦测 (RAW 旁路前递动态化)
    // ==========================================
    // ★ 严格对齐 BRAM 的保持特性
    val r_index_reg = RegEnable(io.r_index, io.r_en) 
    val r_en_reg    = RegNext(io.r_en, false.B)

    // Data 专属冲突追踪 (★ 逻辑后移：先打拍，后比较！)
    val w_data_en_reg    = RegNext(io.w_data_en, false.B)
    val w_index_data_reg = RegNext(io.w_index_data)
    val w_way_reg        = RegNext(io.w_way)

    val is_data_conflict = Wire(Vec(p.ways, Bool()))
    for (w <- 0 until p.ways) {
        // 全部使用 _reg 后缀的信号进行比较。
        // 这意味着 io.r_index 将被直接吸入上方的 r_index_reg，彻底切断 LSQ 传来的组合逻辑链！
        is_data_conflict(w) := r_en_reg && w_data_en_reg && (r_index_reg === w_index_data_reg) && (w_way_reg === w.U)
    }
    
    val w_tag_en_reg = RegNext(io.w_tag_en, false.B)
    val w_tag_reg    = RegNext(io.w_tag, 0.U)
    val w_data_reg   = RegNext(io.w_data)
    val w_strb_reg   = RegNext(io.w_strb)

    // ==========================================
    // 读端口逻辑：多路并行读取
    // ==========================================
    for (w <- 0 until p.ways) {
        io.r_valid(w) := valid_array(w)(r_index_reg)
        
        // ★ Tag 旁路补齐 r_en_reg 校验
        val raw_tag = tag_array(w).read(io.r_index, io.r_en)
        val tag_bypass = r_en_reg && w_tag_en_reg && (RegNext(io.w_way) === w.U) && (RegNext(io.w_index_tag) === r_index_reg)
        io.r_tag(w) := Mux(tag_bypass, w_tag_reg, raw_tag)

        // Data 读取及字节级 RAW 旁路
        for (b <- 0 until p.lineWords) {
            val raw_word_banks = data_array(w)(b).read(io.r_index, io.r_en)
            val bytes = Wire(Vec(4, UInt(8.W)))
            for (byte <- 0 until 4) {
                val byte_bypass = is_data_conflict(w) && w_strb_reg(b)(byte)
                bytes(byte) := Mux(byte_bypass, w_data_reg(b)(byte*8+7, byte*8), raw_word_banks(byte))
            }
            io.r_data(w)(b) := Cat(bytes(3), bytes(2), bytes(1), bytes(0))
        }
    }

    // ==========================================
    // 写端口逻辑：利用条件生成路由
    // ==========================================
    when(io.w_valid_en) { 
        // ★ 用 for 循环展开，推断干净的 CE 信号
        for (w <- 0 until p.ways) {
            when(io.w_way === w.U) { valid_array(w)(io.w_index_tag) := io.w_valid_data }
        }
    }

    when(io.w_tag_en) {
        for (w <- 0 until p.ways) {
            when(io.w_way === w.U) { tag_array(w).write(io.w_index_tag, io.w_tag) }
        }
    }

    when(io.w_data_en) {
        for (w <- 0 until p.ways) {
            when(io.w_way === w.U) {
                for (b <- 0 until p.lineWords) {
                    val wdata_word = io.w_data(b)
                    val w_vec = VecInit(wdata_word(7,0), wdata_word(15,8), wdata_word(23,16), wdata_word(31,24))
                    val mask  = VecInit(io.w_strb(b).asBools)
                    data_array(w)(b).write(io.w_index_data, w_vec, mask)
                }
            }
        }
    }
}
// =========================================================================
// CPU 与 Cache 的交互接口 (加入 implicit 参数)
// =========================================================================
class CacheToCpuIO(implicit p: CacheConfig) extends Bundle {
    val valid    = Input(Bool())
    val op       = Input(Bool())
    val req_id   = Input(UInt(9.W)) 
    // ★ 核心修改：位宽全部跟随配置动态变化
    val index    = Input(UInt(p.indexBits.W)) 
    val tag      = Input(UInt(p.tagBits.W))
    val offset   = Input(UInt(p.offsetBits.W))
    
    val wstrb    = Input(UInt(4.W))  // 没变，因为 CPU 依然是一次写 32位(4字节)
    val wdata    = Input(UInt(32.W)) // 没变，CPU 写单字
    val uncached = Input(Bool())
    val cacop_en = Input(Bool())
    val cacop_op = Input(UInt(3.W)) // 注意这里配合你 AguUnit 改成 3 位
    
    val addr_ok  = Output(Bool())
    val data_ok  = Output(Bool())
    val ret_id   = Output(UInt(9.W))
    val rdata    = Output(UInt(64.W)) // 没变，双发射依然一次拿 64 位
}

// =========================================================================
// MSHR 状态机枚举
// =========================================================================
object MshrState extends ChiselEnum {
    val invalid    = Value // 空闲可用
    val wait_evict = Value // 等待向 AXI 写回脏数据
    val wait_ar    = Value // 等待 AXI 接收读请求 (AR)
    val wait_r     = Value // 等待 AXI 返回读数据 (R Burst)
    val refill     = Value // 数据收齐，写入 1R1W 物理阵列
    val wake_up    = Value // 【全新】挨个向 LSQ 发送 data_ok 唤醒子项
}

// =========================================================================
// MSHR 子项 (Sub-Entry) 
// 记录合并到同一个 MSHR 上的多次同地址请求
// =========================================================================
class MshrSubEntry(implicit p: CacheConfig) extends Bundle {
    val valid  = Bool()
    val req_id = UInt(9.W)
    val op     = Bool()
    // ★ 致命修复：这里必须是 p.offsetBits.W，千万不能是 4.W！
    val offset = UInt(p.offsetBits.W) 
    val wstrb  = UInt(4.W)
    val wdata  = UInt(32.W)
}

// =========================================================================
// MSHR 主条目 (Mshr Entry)
// =========================================================================
class MshrEntry(implicit p: CacheConfig) extends Bundle {
    val state       = MshrState()
    
    // 物理地址核心信息
    val is_uncached = Bool()
    val is_cacop    = Bool()
    val cacop_op    = UInt(3.W)
    val tag         = UInt(p.tagBits.W)  // ★ 动态位宽
    val index       = UInt(p.indexBits.W) // ★ 动态位宽
    val way         = UInt(p.wayBits.W)   // ★ 参数化路索引 (2路就是1位，4路就是2位)
    
    val sub_entries = Vec(4, new MshrSubEntry()) 
    
    // 脏数据驱逐（Eviction）信息
    val victim_v    = Bool()
    val victim_d    = Bool()
    val victim_tag  = UInt(p.tagBits.W) // ★ 动态位宽
    
    // AXI 数据接收缓冲区
    val recv_cnt    = UInt(log2Ceil(p.lineWords + 1).W) // ★ 动态计数器位宽
    val line_buffer = Vec(p.lineWords, UInt(32.W))      // ★ 支持 8个字 (32 Bytes)
}

// Cache 与 AXI 转接桥的交互接口 (参考书本表 10.3)
class CacheToAxiIO(implicit p: CacheConfig) extends Bundle {
    val rd_req    = Output(Bool())
    val rd_id     = Output(UInt(4.W))
    val rd_type   = Output(UInt(3.W)) 
    val rd_addr   = Output(UInt(32.W))
    val rd_rdy    = Input(Bool())

    val ret_valid = Input(Bool())
    val ret_id    = Input(UInt(4.W))
    val ret_last  = Input(Bool())
    val ret_data  = Input(UInt(32.W)) // AXI 依然是 32位数据总线

    val wr_req    = Output(Bool())
    val wr_type   = Output(UInt(3.W))
    val wr_addr   = Output(UInt(32.W))
    val wr_wstrb  = Output(UInt(4.W))
    // ★ 核心修改：写回脏数据时，一口气吐出整行！(32 Bytes = 256 bits)
    val wr_data   = Output(UInt((p.lineWords * 32).W)) 
    val wr_rdy    = Input(Bool())
}
class Cache(implicit p: CacheConfig) extends Module {
    val io = IO(new Bundle {
        val cpu = new CacheToCpuIO()
        val axi = new CacheToAxiIO()
    })

    val MSHR_NUM = 4
    val SUB_ENTRY_NUM = 4
    // 注意：这里要给 MshrEntry 传入隐式参数
    val mshr_table = RegInit(VecInit(Seq.fill(MSHR_NUM)(0.U.asTypeOf(new MshrEntry()))))
    
    val sIdle :: sLookup :: Nil = Enum(2)
    val main_state = RegInit(sIdle)
    
    val wbIdle :: wbWrite :: Nil = Enum(2)
    val wb_state = RegInit(wbIdle)

    // 1. 物理层：例化刚刚写好的防弹版 1R1W 阵列
    val array = Module(new CacheArray1R1W())
    
    // ★ 参数化 Dirty 寄存器：生成 p.ways 路，每路 p.sets 个脏位
    val dirty_array = RegInit(VecInit(Seq.fill(p.ways)(VecInit(Seq.fill(p.sets)(false.B)))))

    // --- 请求缓冲 ---
    val req_op       = RegInit(false.B)
    val req_req_id   = RegInit(0.U(9.W))
    val req_index    = RegInit(0.U(p.indexBits.W)) // ★
    val req_tag      = RegInit(0.U(p.tagBits.W))   // ★
    val req_offset   = RegInit(0.U(p.offsetBits.W)) // ★
    val req_wstrb    = RegInit(0.U(4.W))
    val req_wdata    = RegInit(0.U(32.W))
    val req_uncached = RegInit(false.B)
    val req_cacop_en = RegInit(false.B)
    val req_cacop_op = RegInit(0.U(3.W))

    // --- 命中写缓冲 (Delayed Write) ---
    val wb_way   = RegInit(0.U(p.wayBits.W))
    val wb_index = RegInit(0.U(p.indexBits.W))
    val wb_strb  = RegInit(VecInit(Seq.fill(p.lineWords)(0.U(4.W))))  // ★ 动态行长
    val wb_data  = RegInit(VecInit(Seq.fill(p.lineWords)(0.U(32.W)))) // ★ 动态行长

    // =========================================================================
    // 2. 查表与命中判定 (完全向量化)
    // =========================================================================
    val can_accept = !io.cpu.cacop_en 
    val is_accepting = io.cpu.valid && io.cpu.addr_ok
    
    array.io.r_en    := is_accepting || (main_state === sLookup)
    array.io.r_index := Mux(is_accepting, io.cpu.index, req_index)

    // ★ 动态向量化：收集各路 Hit 和 Dirty 状态
    val way_v   = Wire(Vec(p.ways, Bool()))
    val way_tag = Wire(Vec(p.ways, UInt(p.tagBits.W)))
    val way_hit = Wire(Vec(p.ways, Bool()))
    val d_val   = Wire(Vec(p.ways, Bool())) // 前递后的脏位

    for (w <- 0 until p.ways) {
        way_v(w)   := array.io.r_valid(w)
        way_tag(w) := array.io.r_tag(w)
        way_hit(w) := (!req_uncached || req_cacop_en) && way_v(w) && (way_tag(w) === req_tag)
        
        // ★ 保留原味防线：如果前一拍命中写正在 wbWrite 阶段，直接旁路刚置位的脏标志！
        d_val(w)   := Mux(wb_state === wbWrite && wb_way === w.U && wb_index === req_index, true.B, dirty_array(w)(req_index))
    }

    val cache_hit = way_hit.asUInt.orR && (!req_uncached || req_cacop_en)
    val hit_way   = OHToUInt(way_hit) // 命中时的路号 (One-Hot 编码安全转 UInt)
    // 极速版：独热码按位与，绕开所有编码器！
    val hit_dirty = cache_hit && (way_hit.asUInt & d_val.asUInt).orR

    // 针对“索引型 CACOP”，计算目标路是否有效且脏
    // 4 路配置下，通常由虚拟地址的低位 (offset 甚至 tag 的低位) 来选择 way
    // 这里使用 offset 的低 p.wayBits 位作为路索引，完美兼容原本 2 路用 offset(0) 的逻辑
    val cacop_target_way = req_offset(p.wayBits - 1, 0)
    val target_way_v     = way_v(cacop_target_way)
    val target_way_d     = d_val(cacop_target_way)
    val target_way_tag   = way_tag(cacop_target_way)
    val index_needs_wb   = target_way_v && target_way_d

   // =========================================================================
    // 3. MSHR 探针与子项 (Sub-Entry) 状态计算
    // =========================================================================
    val mshr_match_vec    = Wire(Vec(MSHR_NUM, Bool()))
    val mshr_conflict_vec = Wire(Vec(MSHR_NUM, Bool())) // ★ 新增：时序与属性冲突检测
    val mshr_free_vec     = Wire(Vec(MSHR_NUM, Bool()))

    for (i <- 0 until MSHR_NUM) {
        val state = mshr_table(i).state
        val valid_entry = state =/= MshrState.invalid
        val addr_match = valid_entry && (mshr_table(i).tag === req_tag) && (mshr_table(i).index === req_index)
        
        // ★ 核心修复：允许合并的安全窗口仅在 AXI 传输完成前
        val safe_to_merge = (state === MshrState.wait_evict) || (state === MshrState.wait_ar) || (state === MshrState.wait_r)
        
        mshr_match_vec(i) := addr_match && safe_to_merge && !mshr_table(i).is_uncached && !req_uncached && !mshr_table(i).is_cacop
        
        // ★ 终极修复：冲突判定不仅包含“时序窗口关闭”，还必须包含“属性互斥”！
        val is_timing_conflict = addr_match && !safe_to_merge
        val is_attr_conflict   = addr_match && (mshr_table(i).is_uncached || req_uncached || mshr_table(i).is_cacop)
        mshr_conflict_vec(i) := is_timing_conflict || is_attr_conflict
        
        mshr_free_vec(i)  := !valid_entry
    }

    val has_match    = mshr_match_vec.asUInt.orR
    val has_conflict = mshr_conflict_vec.asUInt.orR // ★ 新增
    val match_idx    = PriorityEncoder(mshr_match_vec)
    val matched_mshr = mshr_table(match_idx)

    // 探查匹配到的 MSHR 是否还有空的子项口袋
    val sub_free_vec = Wire(Vec(SUB_ENTRY_NUM, Bool()))
    for (j <- 0 until SUB_ENTRY_NUM) { sub_free_vec(j) := !matched_mshr.sub_entries(j).valid }
    val has_free_sub  = sub_free_vec.asUInt.orR
    val alloc_sub_idx = PriorityEncoder(sub_free_vec)

    // 探查是否有完全空闲的 MSHR
    val has_free_mshr  = mshr_free_vec.asUInt.orR
    val alloc_mshr_idx = PriorityEncoder(mshr_free_vec)

    // ★ 斩断 cache_hit 依赖！悲观预测：只要 Lookup 里是个 Store，直接算作 hazard 风险，不等 SRAM 查表！
    val is_lookup_write = (main_state === sLookup) && req_op
    // =========================================================================
    // 替换二：解除普通指令封锁，利用旁路火力全开
    // =========================================================================
    // 1. 结构冲突检测：普通查表绝不阻塞！仅拦截企图劫持 w_way 控制权的 CACOP
    // =========================================================================
    // 替换二：修复 Cache 内部 RAW 冒险，完美对接 1R1W 旁路
    // =========================================================================
    // 当 sLookup 里是一个 Hit Store 时，它的数据还挂在寄存器里，下一拍才写 SRAM。
    // 此时绝对不能放同地址的 Load 进来读 SRAM，否则会读到旧数据（读比写早了一拍）！
    // 必须让 Load 阻塞 1 拍。下一拍 Store 进入 wbWrite，Load 进入 SRAM 读取，完美触发 1R1W 旁路前递！
    val hazard_with_lookup = is_lookup_write && (io.cpu.index === req_index)
    // 修复前：只拦截 CACOP，导致普通访存长驱直入
    // val hazard_with_wb = (wb_state === wbWrite) && io.cpu.cacop_en

    // ★ 修复后：只要组号 (Index) 撞车，或者是 CACOP，统统给我阻塞一拍！
    val hazard_with_wb = (wb_state === wbWrite) && ((io.cpu.index === wb_index) || io.cpu.cacop_en)
    val hit_write_hazard   = hazard_with_lookup || hazard_with_wb

    // 2. 前台阻塞逻辑
    val mshr_full_block = !has_match && !has_free_mshr
    val sub_full_block  = has_match && !has_free_sub
    val cacop_block     = req_cacop_en && (mshr_table.map(_.state =/= MshrState.invalid).reduce(_ || _) || wb_state === wbWrite)
    val block_frontend  = mshr_full_block || sub_full_block || cacop_block || has_conflict

    // 3. 修复 addr_ok：
    val can_accept_new = !hit_write_hazard && !block_frontend
    val lookup_accept_normal = !req_cacop_en 
    io.cpu.addr_ok := can_accept_new && ((main_state === sIdle) || (main_state === sLookup && lookup_accept_normal))

    // =========================================================================
    // 4. 前端状态机 (主控)
    // =========================================================================
    when(is_accepting) {
        req_op       := io.cpu.op
        req_req_id   := io.cpu.req_id
        req_index    := io.cpu.index
        req_tag      := io.cpu.tag
        req_offset   := io.cpu.offset
        req_wstrb    := io.cpu.wstrb
        req_wdata    := io.cpu.wdata
        req_uncached := io.cpu.uncached
        req_cacop_en := io.cpu.cacop_en
        req_cacop_op := io.cpu.cacop_op
    }

    // =========================================================================
    // 工业级替换算法：Tree-PLRU (伪 LRU) - 专为 4 路优化
    // =========================================================================
    // 4路只需 3 bits/set。Bit0=Root, Bit1=Left, Bit2=Right
    val plru_array = RegInit(VecInit(Seq.fill(p.sets)(0.U(3.W))))
    
    // 1. 读取当前 Set 的 PLRU 状态，计算出哪一路是最老的 (Victim)
    val current_plru = plru_array(req_index)
    val plru_victim_way = Wire(UInt(p.wayBits.W))
    
    if (p.ways == 4) {
        when(current_plru(0) === 0.U) {
            plru_victim_way := Mux(current_plru(1) === 0.U, 0.U, 1.U)
        } .otherwise {
            plru_victim_way := Mux(current_plru(2) === 0.U, 2.U, 3.U)
        }
    } else {
        // 如果你改回 2 路，自动降级为 1-bit LRU
        plru_victim_way := current_plru(0)
    }

    // =========================================================================
    // 替换三：修复 Victim 脏位绑定与替换策略 (优先空闲，其次 PLRU)
    // =========================================================================
    val invalid_way_idx = PriorityEncoder(~way_v.asUInt)
    val has_invalid_way = (~way_v.asUInt).orR
    
    // ★ 用 Tree-PLRU 踢掉最老的路
    val target_fill_way = Mux(has_invalid_way, invalid_way_idx, plru_victim_way)
    
    val victim_v   = way_v(target_fill_way)
    val victim_t   = way_tag(target_fill_way)
    val victim_drt = d_val(target_fill_way)
    val need_evict = !req_uncached && victim_v && victim_drt

    
    switch(main_state) {
        is(sIdle) {
            when(is_accepting) { main_state := sLookup }
        }
        is(sLookup) {
            when(req_cacop_en) {
                when(!block_frontend) {
                    val cacop_need_wb = (req_cacop_op === 1.U && index_needs_wb) ||
                                        (req_cacop_op === 2.U && hit_dirty)
                    when(cacop_need_wb) {
                        val m = mshr_table(alloc_mshr_idx)
                        // ★ 动态路选择
                        val target_way = Mux(req_cacop_op === 2.U, hit_way, cacop_target_way)
                        
                        m.state       := MshrState.wait_evict
                        m.is_cacop    := true.B
                        m.is_uncached := false.B
                        m.tag         := req_tag
                        m.index       := req_index
                        m.way         := target_way
                        m.victim_v    := true.B
                        m.victim_tag  := Mux(req_cacop_op === 1.U, target_way_tag, req_tag)
                        m.victim_d    := true.B

                        m.sub_entries(0).valid  := true.B
                        m.sub_entries(0).req_id := req_req_id 
                        m.sub_entries(0).op     := false.B
                        for (j <- 1 until SUB_ENTRY_NUM) { m.sub_entries(j).valid := false.B }
                        
                        // ★ 核心修复：利用向量化的 r_data 提取旧数据
                        // ★ 核心修复 1：CACOP 驱逐时的 Store 旁路拯救
                        val old_line_cacop = array.io.r_data(target_way)
                        for(b <- 0 until p.lineWords) { 
                            val is_wb_overlap = (wb_state === wbWrite) && (wb_index === req_index) && (wb_way === target_way)
                            val raw_word = old_line_cacop(b)
                            val wb_word  = wb_data(b)
                            val wb_mask  = wb_strb(b)
                            val merged_word = Wire(Vec(4, UInt(8.W)))
                            for (byte <- 0 until 4) {
                                merged_word(byte) := Mux(is_wb_overlap && wb_mask(byte), wb_word(byte*8+7, byte*8), raw_word(byte*8+7, byte*8))
                            }
                            m.line_buffer(b) := merged_word.asUInt
                        }
                        
                        main_state := Mux(is_accepting, sLookup, sIdle) 
                    } .otherwise {
                        main_state := Mux(is_accepting, sLookup, sIdle)
                    }
                }
            } .elsewhen(cache_hit) {
                main_state := Mux(is_accepting, sLookup, sIdle)
            } .elsewhen(!block_frontend) {
                when(has_match) {
                    val sub = matched_mshr.sub_entries(alloc_sub_idx)
                    sub.valid  := true.B
                    sub.req_id := req_req_id
                    sub.op     := req_op
                    sub.offset := req_offset
                    sub.wstrb  := req_wstrb
                    sub.wdata  := req_wdata
                    main_state := Mux(is_accepting, sLookup, sIdle)
                } .otherwise {
                    val m = mshr_table(alloc_mshr_idx)
                    m.state       := Mux(need_evict || (req_uncached && req_op), MshrState.wait_evict, MshrState.wait_ar)
                    m.is_cacop    := false.B
                    m.is_uncached := req_uncached
                    m.tag         := req_tag
                    m.index       := req_index
                    m.way         := target_fill_way
                    m.victim_v    := victim_v
                    m.victim_tag  := victim_t
                    m.victim_d    := victim_drt
                    m.recv_cnt    := 0.U

                    m.sub_entries(0).valid  := true.B
                    m.sub_entries(0).req_id := req_req_id
                    m.sub_entries(0).op     := req_op
                    m.sub_entries(0).offset := req_offset
                    m.sub_entries(0).wstrb  := req_wstrb
                    m.sub_entries(0).wdata  := req_wdata
                    for (j <- 1 until SUB_ENTRY_NUM) { m.sub_entries(j).valid := false.B }

                    // ★ 核心修复：提取牺牲路的旧数据用于写回
                    // ★ 核心修复 2：普通 Miss 驱逐时的 Store 旁路拯救
                    val old_line_miss = array.io.r_data(target_fill_way)
                    for(b <- 0 until p.lineWords) { 
                        val is_wb_overlap = (wb_state === wbWrite) && (wb_index === req_index) && (wb_way === target_fill_way)
                        val raw_word = old_line_miss(b)
                        val wb_word  = wb_data(b)
                        val wb_mask  = wb_strb(b)
                        val merged_word = Wire(Vec(4, UInt(8.W)))
                        for (byte <- 0 until 4) {
                            merged_word(byte) := Mux(is_wb_overlap && wb_mask(byte), wb_word(byte*8+7, byte*8), raw_word(byte*8+7, byte*8))
                        }
                        m.line_buffer(b) := merged_word.asUInt
                    }
                    
                    main_state := Mux(is_accepting, sLookup, sIdle)
                }
            }
        }
    }

    // =========================================================================
    // 5. 命中写回缓冲 (wbWrite) 与 Dirty 位更新
    // =========================================================================
    // ★ 动态词偏移索引提取 (支持任意行长)
    val word_offset = req_offset(p.offsetBits - 1, 2)

    switch(wb_state) {
        is(wbIdle) {
            when(main_state === sLookup && cache_hit && req_op && !req_uncached) {
                wb_state := wbWrite
                wb_way   := hit_way
                wb_index := req_index
                for (b <- 0 until p.lineWords) {
                    wb_strb(b) := Mux(word_offset === b.U, req_wstrb, 0.U(4.W))
                    wb_data(b) := req_wdata
                }
            }
        }
        is(wbWrite) {
            when(main_state === sLookup && cache_hit && req_op && !req_uncached) {
                wb_state := wbWrite
                wb_way   := hit_way
                wb_index := req_index
                for (b <- 0 until p.lineWords) {
                    wb_strb(b) := Mux(word_offset === b.U, req_wstrb, 0.U(4.W))
                    wb_data(b) := req_wdata
                }
            } .otherwise { wb_state := wbIdle }
        }
    }

    when(wb_state === wbWrite) {
        // ★ 核心修复：展开二维数组赋值，提供完美的 CE 信号
        for (w <- 0 until p.ways) {
            when(wb_way === w.U) { dirty_array(w)(wb_index) := true.B }
        }
    }

    // =========================================================================
    // 6. MSHR 异步回填引擎 (Refill) 完美折叠
    // =========================================================================
    val is_refill_vec = mshr_table.map(_.state === MshrState.refill)
    val has_refill = is_refill_vec.reduce(_ || _)
    val refill_idx = PriorityEncoder(is_refill_vec)
    val m_ref = mshr_table(refill_idx)

    val final_merged_data = WireDefault(m_ref.line_buffer)
    val final_merged_strb = WireDefault(VecInit(Seq.fill(p.lineWords)("hf".U(4.W))))

    // ★ 时空折叠：适配 8 字行长
    for (b <- 0 until p.lineWords) {
        var current_word = m_ref.line_buffer(b)
        for (j <- 0 until SUB_ENTRY_NUM) {
            val sub = m_ref.sub_entries(j)
            val sub_word_idx = sub.offset(p.offsetBits - 1, 2)
            val is_target_bank = (sub_word_idx === b.U)
            val apply_store = sub.valid && sub.op && is_target_bank
            
            val next_word = Wire(Vec(4, UInt(8.W)))
            val curr_bytes = VecInit(current_word(7,0), current_word(15,8), current_word(23,16), current_word(31,24))
            for (byte <- 0 until 4) {
                next_word(byte) := Mux(apply_store && sub.wstrb(byte), sub.wdata(byte*8+7, byte*8), curr_bytes(byte))
            }
            current_word = next_word.asUInt
        }
        final_merged_data(b) := current_word
    }

    // --- 连线至 1R1W 阵列的写入端口 ---
    // CACOP 判定依然依赖于请求状态，确保与 4 路对应
    val cacop_inval_target = (main_state === sLookup) && req_cacop_en && !block_frontend && !has_refill && (
        (req_cacop_op === 0.U) || 
        (req_cacop_op === 1.U) || 
        (req_cacop_op === 2.U && cache_hit)
    )
    val is_cacop_write = cacop_inval_target
    val cacop_w_way = Mux(req_cacop_op === 2.U, hit_way, cacop_target_way)

    val do_refill = has_refill && (wb_state =/= wbWrite) 
    val do_refill_write_ram = do_refill && !m_ref.is_uncached 

    array.io.w_way         := Mux(is_cacop_write, cacop_w_way, Mux(do_refill_write_ram, m_ref.way, wb_way))

    // Valid 和 Tag 分离写控制
    array.io.w_valid_en    := do_refill_write_ram || is_cacop_write
    array.io.w_valid_data  := Mux(do_refill_write_ram, true.B, false.B) // 回填为 1，CACOP 清空为 0
    
    array.io.w_tag_en      := do_refill_write_ram || is_cacop_write
    array.io.w_index_tag   := Mux(is_cacop_write, req_index, Mux(do_refill_write_ram, m_ref.index, wb_index))
    array.io.w_tag         := Mux(do_refill_write_ram, m_ref.tag, 0.U) 

    array.io.w_data_en     := do_refill_write_ram || (wb_state === wbWrite)
    array.io.w_index_data  := Mux(do_refill_write_ram, m_ref.index, wb_index)
    array.io.w_data        := Mux(do_refill_write_ram, final_merged_data, wb_data)
    array.io.w_strb        := Mux(do_refill_write_ram, final_merged_strb, wb_strb)

    val has_store_sub = m_ref.sub_entries.map(s => s.valid && s.op).reduce(_ || _)
    when(do_refill) {
        when(!m_ref.is_uncached) {
            // ★ 核心修复：同样在回填时展开 Dirty 赋值
            for (w <- 0 until p.ways) {
                when(m_ref.way === w.U) { dirty_array(w)(m_ref.index) := has_store_sub }
            }
            for(b <- 0 until p.lineWords) { m_ref.line_buffer(b) := final_merged_data(b) }
        }
        m_ref.state := MshrState.wake_up 
    }
    // =========================================================================
    // ★ 新增：Tree-PLRU 状态更新逻辑
    // =========================================================================
    // 只有两种情况代表真正“访问”了 Cache：
    // 1. Lookup 阶段命中 (Hit)
    // 2. 发生 Miss，从 AXI 搬回数据后写回 SRAM (Refill)
    val do_plru_update = (main_state === sLookup && cache_hit && !req_uncached && !req_cacop_en) || do_refill
    val access_way = Mux(do_refill, m_ref.way, hit_way)
    val access_idx = Mux(do_refill, m_ref.index, req_index)

    when(do_plru_update) {
        val old_plru = plru_array(access_idx)
        val new_plru = WireDefault(old_plru)
        
        if (p.ways == 4) {
            // 被访问的路，其对应的树枝全部指向反方向 (避开最新访问的路)
            switch(access_way) {
                is(0.U) { new_plru := Cat(old_plru(2), 1.U(1.W), 1.U(1.W)) } // 00 -> 根右指, 左枝右指
                is(1.U) { new_plru := Cat(old_plru(2), 0.U(1.W), 1.U(1.W)) } // 01 -> 根右指, 左枝左指
                is(2.U) { new_plru := Cat(1.U(1.W), old_plru(1), 0.U(1.W)) } // 10 -> 根左指, 右枝右指
                is(3.U) { new_plru := Cat(0.U(1.W), old_plru(1), 0.U(1.W)) } // 11 -> 根左指, 右枝左指
            }
        } else {
            new_plru := ~access_way(0)
        }
        plru_array(access_idx) := new_plru
    }


    // =========================================================================
    // 7. 乱序唤醒引擎 (Wake Up LSQ) 与 完美数据选择
    // =========================================================================
    val is_wakeup_vec = mshr_table.map(_.state === MshrState.wake_up)
    val has_wakeup = is_wakeup_vec.reduce(_ || _)
    val wakeup_mshr_idx = PriorityEncoder(is_wakeup_vec)
    val m_wake = mshr_table(wakeup_mshr_idx)

    val sub_wakeup_vec = Wire(Vec(SUB_ENTRY_NUM, Bool()))
    for (j <- 0 until SUB_ENTRY_NUM) { sub_wakeup_vec(j) := m_wake.sub_entries(j).valid }
    val has_valid_sub = sub_wakeup_vec.asUInt.orR
    val wakeup_sub_idx = PriorityEncoder(sub_wakeup_vec)
    val active_sub = m_wake.sub_entries(wakeup_sub_idx)

    // 1. 提取词偏移索引 (与原来一样)
    val req_word_idx = req_offset(p.offsetBits - 1, 2)
    val is_req_last_word = (req_word_idx === (p.lineWords - 1).U)
    val safe_req_next_idx = Mux(is_req_last_word, 0.U, req_word_idx + 1.U)

    // ★ 优化核心：先从所有路中把目标 Word 挑出来 (与 Tag 比较并行！)
    val way_words0 = Wire(Vec(p.ways, UInt(32.W)))
    val way_words1 = Wire(Vec(p.ways, UInt(32.W)))
    for (w <- 0 until p.ways) {
        way_words0(w) := array.io.r_data(w)(req_word_idx)
        way_words1(w) := Mux(is_req_last_word, 0.U, array.io.r_data(w)(safe_req_next_idx))
    }
    
    // ★ 砍掉 OHToUInt！当 way_hit (Tag比较) 出结果时，直接 Mux1H 吐出数据
    val sram_hit_word0 = Mux1H(way_hit, way_words0)
    val sram_hit_word1 = Mux1H(way_hit, way_words1)
    
    val mshr_word_idx = active_sub.offset(p.offsetBits - 1, 2)
    val mshr_bypass_word0 = m_wake.line_buffer(mshr_word_idx)
    
    // ★ 同样为 MSHR 旁路提供越界保护
    val is_mshr_last_word = (mshr_word_idx === (p.lineWords - 1).U)
    val safe_mshr_next_idx = Mux(is_mshr_last_word, 0.U, mshr_word_idx + 1.U)
    val mshr_bypass_word1 = Mux(is_mshr_last_word, 0.U, m_wake.line_buffer(safe_mshr_next_idx))

    val hit_response = main_state === sLookup && cache_hit && !req_cacop_en
    val cacop_index_done = (main_state === sLookup) && req_cacop_en && !block_frontend && ((req_cacop_op === 0.U) || (req_cacop_op === 1.U && !index_needs_wb))
    val cacop_hit_inval_done = (main_state === sLookup) && req_cacop_en && !block_frontend && (req_cacop_op === 2.U) && !hit_dirty
    
    val any_front_response = hit_response || cacop_index_done || cacop_hit_inval_done

    io.cpu.data_ok := any_front_response || (has_wakeup && has_valid_sub)
    io.cpu.ret_id  := Mux(any_front_response, req_req_id, active_sub.req_id)
    
    val final_word0 = Mux(hit_response, sram_hit_word0, Mux(m_wake.is_uncached, m_wake.line_buffer(0), mshr_bypass_word0))
    val final_word1 = Mux(hit_response, sram_hit_word1, Mux(m_wake.is_uncached, 0.U, mshr_bypass_word1))

    io.cpu.rdata := Cat(final_word1, final_word0) 

    when(has_wakeup && has_valid_sub && !any_front_response) {
        active_sub.valid := false.B
        when(PopCount(sub_wakeup_vec.asUInt) === 1.U) { m_wake.state := MshrState.invalid }
    }

    // =========================================================================
    // 8. 缺少的 AXI 异步总线引擎 (物流系统)
    // =========================================================================
    // --- A) 写通道 (Eviction & Uncached Write) ---
    val evict_cands = mshr_table.map(_.state === MshrState.wait_evict)
    val do_evict = evict_cands.reduce(_ || _)
    val evict_idx = PriorityEncoder(evict_cands)
    val m_ev = mshr_table(evict_idx)

    io.axi.wr_req  := do_evict
    io.axi.wr_type := Mux(m_ev.is_uncached, 2.U, 4.U)
    // ★ 核心修复：物理地址拼接时的 0.U 位宽必须随 offsetBits 动态改变！
    io.axi.wr_addr := Mux(m_ev.is_uncached, 
                          Cat(m_ev.tag, m_ev.index, m_ev.sub_entries(0).offset), 
                          Cat(m_ev.victim_tag, m_ev.index, 0.U(p.offsetBits.W)))
    
    io.axi.wr_wstrb:= Mux(m_ev.is_uncached, m_ev.sub_entries(0).wstrb, "hf".U)
    // ★ 核心修复：完美利用 Scala 集合的 reverse 与 Cat，一行拼接任意行长的 256 位大总线
    io.axi.wr_data := Mux(m_ev.is_uncached, 
                          Fill(p.lineWords, m_ev.sub_entries(0).wdata), 
                          Cat(m_ev.line_buffer.reverse))

    when(do_evict && io.axi.wr_rdy) {
        val is_uncached_store = m_ev.is_uncached && m_ev.sub_entries(0).op
        m_ev.state := Mux(is_uncached_store || m_ev.is_cacop, MshrState.wake_up, MshrState.wait_ar)
    }

    // --- B) 读地址通道 (AR) ---
    val ar_cands = mshr_table.map(_.state === MshrState.wait_ar)
    val do_ar = ar_cands.reduce(_ || _)
    val ar_idx = PriorityEncoder(ar_cands)
    val m_ar = mshr_table(ar_idx)

    io.axi.rd_req  := do_ar
    io.axi.rd_id   := ar_idx  
    io.axi.rd_type := Mux(m_ar.is_uncached, 2.U, 4.U)
    // ★ 核心修复：读地址拼接时的 0.U 位宽同样动态改变
    io.axi.rd_addr := Cat(m_ar.tag, m_ar.index, Mux(m_ar.is_uncached, m_ar.sub_entries(0).offset, 0.U(p.offsetBits.W)))

    when(do_ar && io.axi.rd_rdy) {
        m_ar.state := MshrState.wait_r
    }

    // --- C) 读数据通道 (R) 收快递 ---
    when(io.axi.ret_valid) {
        val ret_idx = io.axi.ret_id(1, 0) // 还原 MSHR Index
        val m_r = mshr_table(ret_idx)
        
        // ★ 修复警告：强行截取合法索引位宽 (对于 8 字长，截取 2:0)
        val safe_recv_idx = m_r.recv_cnt(log2Ceil(p.lineWords) - 1, 0)
        m_r.line_buffer(safe_recv_idx) := io.axi.ret_data
        
        when(io.axi.ret_last) {
            // 收齐后，走 1R1W 写端口的回填逻辑
            m_r.state := MshrState.refill
        } .otherwise {
            m_r.recv_cnt := m_r.recv_cnt + 1.U
        }
    }
}
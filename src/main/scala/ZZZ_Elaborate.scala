package mycpu

import circt.stage.ChiselStage

object Elaborate extends App {
    ChiselStage.emitSystemVerilogFile(
        new core_top(),
        firtoolOpts = Array(
            "-disable-all-randomization", 
            "-strip-debug-info"
        )
    )
}
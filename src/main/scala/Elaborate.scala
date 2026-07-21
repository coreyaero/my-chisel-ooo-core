package mycpu

import circt.stage.ChiselStage

object Elaborate extends App {
    ChiselStage.emitSystemVerilogFile(
        new mycpu_top(),
        firtoolOpts = Array(
            "-disable-all-randomization", 
            "-strip-debug-info"
        )
    )
}
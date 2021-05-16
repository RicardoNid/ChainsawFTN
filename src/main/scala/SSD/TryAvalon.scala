package SSD

import spinal.core._
import spinal.lib._
import spinal.lib.bus.avalon.AvalonMM

class TryAvalon extends Component {
  val interface0 = slave(AvalonMM(interfaceConfig))
  val interface1 = master(AvalonMM(interfaceConfig))
  interface1 <> interface0
}

object TryAvalon {
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new TryAvalon)
  }
}

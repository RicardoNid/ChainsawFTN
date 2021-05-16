import Chainsaw._
import spinal.core._
import spinal.core.sim._

class HelloChainsaw extends Component {

  val input0 = in(SIntReal(-2, 2))
  val input1 = in(SIntReal(-2, 2))
  val output = input0 + input1
  out(output)

}

object HelloChainsaw { // this is a helloworld!
  def main(args: Array[String]): Unit = {
    SpinalConfig().generateSystemVerilog(new HelloChainsaw)
    SimConfig.withWave.compile(new HelloChainsaw).doSim{dut =>
      dut.input0 #= 1
      dut.input1 #= 2
      sleep(1)
      println(dut.output.toDouble)
    }
  }
}



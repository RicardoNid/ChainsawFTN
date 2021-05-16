import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.io.Source

//串行NTT器件
//NTT模数3329
//NTT原根3
//NTT底数3061
//iNTT底数2298
//2^12 mod 3329=767

object getNTTnumber { //得到NTT所需数据的数组
  def apply(): Array[Bits] = {
    val output = new Array[Bits](128)
    var temp = 767
    for (i <- 0 until 128) {
      output(i) = B(temp, 12 bits)
      temp = (temp * 3061) % 3329
    }
    output
  }
}

object getiNTTnumber { //得到iNTT所需数据的数组
  def apply(): Array[Bits] = {
    val output = new Array[Bits](128)
    var temp = 767
    for (i <- 0 until 128) {
      output(i) = B(temp, 12 bits)
      temp = (temp * 2298) % 3329
    }
    output
  }
}

class NTTserial() extends Component {

  val io = new Bundle {

    val addr_in = in UInt (8 bits) //输入地址
    val data_in = in Bits (12 bits) //输入数据
    val addr_out = in UInt (8 bits) //输出地址
    val data_out = out Bits (12 bits) //输出数据
    val cond = in Bool() //状态，0代表输入态，1代表计算态
    val end = out Bool() //计算态的最后一个周期会变高以示计算完成
    val en = in Bool() //使能

  }

  val NTTnumber = Mem(Bits(12 bits), getNTTnumber()) //计算NTT所需的数据

  val c = new serialFFToperator(8, Bits(12 bits), Bits(12 bits), ButterflyOperator)

  c.io.data_butterflyNum := NTTnumber(c.io.addr_butterflyNum)

  c.io.addr_in := io.addr_in
  c.io.data_in := io.data_in
  c.io.addr_out := io.addr_out
  io.data_out := c.io.data_out
  c.io.cond := io.cond
  io.end := c.io.end
  c.io.en := io.en

  def ButterflyOperator(A: Bits, B: Bits, C: Bits): (Bits, Bits) = {
    val c = new NTTButterfly(3329)
    c.io.A := B
    c.io.B := C
    c.io.C := A
    (c.io.D, c.io.E)
  }

}

object NTTserialtst {
  def main(arg: Array[String]): Unit = {
    SpinalVhdl(new NTTserial())
  }
}

object NTTserialsim {
  def main(arg: Array[String]): Unit = {
    val file = Source.fromFile("测试数据.txt")
    val It = file.getLines
    val n = 10000 //测试次数
    var temp = 0

    SimConfig.withWave.compile(new NTTserial()).doSimUntilVoid { dut =>

      dut.clockDomain.forkStimulus(10)

      fork {
        dut.io.en #= true //使能
        dut.io.cond #= false //输入状态
        for (i <- 0 until n) {
          for (j <- 0 until 256) {
            dut.io.addr_in #= j
            dut.io.data_in #= It.next().toInt
            dut.clockDomain.waitSampling()
          }
          dut.io.cond #= true //计算状态
          do {
            dut.clockDomain.waitSampling()
          } while (!dut.io.end.toBoolean)
          dut.io.en #= false //使不能
          for (j <- 0 until 256) {
            dut.io.addr_out #= j
            dut.clockDomain.waitSampling()
            temp = It.next().toInt
            if (dut.io.data_out.toInt != temp) {
              printf("出错，输出的结果是%d，但实际结果应该是%d。\n", dut.io.data_out.toInt, temp)
            }
          }
          dut.io.en #= true //使能
          dut.io.cond #= false //输入状态
        }
        printf("仿真结束。\n")
        simSuccess()
      }
    }
    file.close
  }
}
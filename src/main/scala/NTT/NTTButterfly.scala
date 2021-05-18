import spinal.core._
import spinal.core.sim._
import scala.language.postfixOps
import scala.util.Random._

//NTT专用Butterfly
//输出的结果不除2

class NTTButterfly(N: BigInt /*被模常数*/) extends Component {

  val n = log2Up(N)

  val io = new Bundle {

    val A = in UInt (n bits)
    val B = in UInt (n bits)
    val C = in UInt (n bits)
    val D = out UInt (n bits) //D=C+A*B*2^-n
    val E = out UInt (n bits) //E=C-A*B*2^-n

  }

  val my_tree = MontMul(io.A, io.B, N)
  val my_tree1 = WallaceTree().add(io.C).add(my_tree)
  val my_tree4 = WallaceTree().add(io.C).sub(my_tree)
  val my_tree2 = my_tree1.copy().insert(-N, 0)
  val my_tree3 = my_tree1.copy().insert(-2 * N, 0)
  val my_tree5 = my_tree4.copy().insert(N, 0)
  val my_tree6 = my_tree4.copy().insert(2 * N, 0)
  val output1 = my_tree1.part().full().conv_Bits(0, n).asUInt
  val output2 = my_tree2.part().full().conv_Bits(0, n + 1).asUInt
  val output3 = my_tree3.part().full().conv_Bits(0, n + 2).asUInt
  val output4 = my_tree4.part().full().conv_Bits(0, n + 2).asUInt
  val output5 = my_tree5.part().full().conv_Bits(0, n + 1).asUInt
  val output6 = my_tree6.part().full().conv_Bits(0, n).asUInt

  when(output3(n + 1) | output3(n)) {
    when(output2(n)) {
      io.D := output1
    }.otherwise {
      io.D := output2.resized
    }
  }.otherwise {
    io.D := output3.resized
  }

  when(output4(n + 1) | output4(n)) {
    when(output5(n)) {
      io.E := output6
    }.otherwise {
      io.E := output5.resized
    }
  }.otherwise {
    io.E := output4.resized
  }
}

object NTTButterflytst {
  def main(arg: Array[String]): Unit = {
    SpinalVhdl(new NTTButterfly(3329))
  }
}

object NTTButterflysim {
  def main(arg: Array[String]): Unit = {
    val n = 100000 //测试次数
    var temp, temp2 = 0
    val N = 3329
    SimConfig.withWave.compile(new NTTButterfly(N))
      .doSim { dut =>
        for (_ <- 0 until n) {
          dut.io.A #= nextInt(N)
          dut.io.B #= nextInt(N)
          dut.io.C #= nextInt(N)
          sleep(1)
          temp = dut.io.A.toInt * dut.io.B.toInt % N
          for (i <- 0 until log2Up(N)) {
            if ((temp & 1) == 1) {
              temp = temp + N
            }
            temp = temp >> 1
          }
          temp2 = (dut.io.C.toInt - temp + N) % N
          temp = (dut.io.C.toInt + temp) % N
          if (temp != dut.io.D.toInt || temp2 != dut.io.E.toInt) {
            printf("出现错误，输入数据为%d，%d和%d，输出结果为%d和%d，但实际结果应该是%d和%d。\n",
              dut.io.A.toInt, dut.io.B.toInt, dut.io.C.toInt, dut.io.D.toInt, dut.io.E.toInt,
              temp, temp2)
          }
        }
        printf("仿真结束。\n")
        simSuccess()
      }
  }
}
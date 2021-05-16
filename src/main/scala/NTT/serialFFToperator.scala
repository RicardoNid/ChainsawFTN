import spinal.core._
import spinal.lib._
import spinal.core.sim._

import scala.io.Source

//FFT操作器件

object reverse { //返序
  def apply(input: UInt): UInt = {
    val output = UInt(input.getWidth bits)
    for (i <- input.range) {
      output(i) := input(input.high - i)
    }
    output
  }
}

class serialFFToperator[T <: Data, T2 <: Data]
(n: Int, /*操作数据的数量为2^n*/ dataType1: HardType[T], dataType2: HardType[T2],
 ButterflyOperator: (T, T, T2) => (T, T)) extends Component {

  val io = new Bundle {

    val addr_in = in UInt (n bits) //输入地址
    val data_in = in(dataType1()) //输入数据
    val addr_out = in UInt (n bits) //输出地址
    val data_out = out(dataType1()) //输出数据
    val addr_butterflyNum = out UInt (n - 1 bits) //蝶形操作所需数据地址输出
    val data_butterflyNum = in(dataType2()) //蝶形操作所需数据输入
    val cond = in Bool() //状态，0代表输入态，1代表计算态
    val end = out Bool() //计算态的最后一个周期会变高以示计算完成
    val en = in Bool() //使能

  }

  val state1 = Reg(UInt(n bits)) init (1) simPublic() //状态机1
  val state2 = Reg(UInt(n - 1 bits)) init (0) simPublic() //状态机2
  val state3 = UInt(n bits) simPublic()
  val state4 = Reg(UInt(n bits)) init (0) simPublic() //状态机4
  val state5 = Reg(UInt(n - 1 bits)) init (0) simPublic() //状态机5
  val data = Reg(Vec(dataType1, 1<<n)) //数据


  io.end := False
  when(state4(n - 2 downto 0).andR) { //结束信号
    io.end := True
  }

  state3 := ~(state1 | U"0" @@ state2)
  when(io.cond === True & io.en === True) { //状态转换
    state4(n - 2 downto 0) := state4(n - 2 downto 0) + 1
    state5 := state5 + reverse(state1).resized
    when((state2 & state4(n - 2 downto 0)) === state2) {
      state4 := ((state4 & state3) + (state1 |<< 1)).resized
      state5 := 0
      when((state3 & state4) === state3) {
        state4 := 0
        state1 := state1.rotateLeft(1)
        state2 := (state2 |<< 1) | 1
        when(state2.andR) {
          state2 := 0
        }
      }
    }
  }

  when(io.cond === False & io.en === True) { //数据输入
    data(reverse(io.addr_in)) := io.data_in
  }

  io.data_out := data(io.addr_out) //数据输出

  io.addr_butterflyNum := state5
  val temp = ButterflyOperator(data(state4), data(state4 | state1), io.data_butterflyNum)

  when(io.cond === True & io.en === True) { //计算
    data(state4) := temp._1
    data(state4 | state1) := temp._2
  }

}
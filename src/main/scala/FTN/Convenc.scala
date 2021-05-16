package FTN

import breeze.numerics.pow
import spinal.core._

class Convenc extends Component {
  import Convenc._
  val io = new Bundle {
    val tocode = in Bits (1 bits)
    val coded = out Bits (2 bits)
  }
  val signals = (0 until 6).map(_ => Bool()).toList ::: List(io.tocode.asBool)
  val trellis1 = trellisGen("133")
  val trellis2 = trellisGen("171")
  //The initial value is 0,if t(i)='1',then reg(length-i) xor with code1, 0 doesn't matter.
  val code0 = Bool()
  val code1 = Bool()
  (0 until io.tocode.getBitsWidth)
    .foreach { i =>
      io.coded(0) := trellis1.zip(signals).filter { case (bit, reg) => bit == '1' }.map(_._2).reduce(_ ^ _)
      io.coded(1) := trellis2.zip(signals).filter { case (bit, reg) => bit == '1' }.map(_._2).reduce(_ ^ _)
      signals.dropRight(1).zip(signals.drop(1)).foreach { case (next, prev) => next := RegNext(prev,init = False) }
    }
}

object Convenc {

  def trellisGen(codgen: String) = {
    val out = (0 until codgen.length).map(i => (codgen(i) - '0') * pow(8, codgen.length - 1 - i)).sum
    out.toBinaryString
  }

  def main(args: Array[String]): Unit = {
    //    val design = new FTN.Convenc
    //    SpinalVerilog(new FTN.Convenc)
    println(trellisGen("133"))

  }
}
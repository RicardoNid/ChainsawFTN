package FTN
import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import spinal.sim._
import Chainsaw.Real
import Chainsaw._
import breeze.numerics.pow
import FTN.Convenc.trellisGen
import scala.util.Random


class Convenc extends Component {
  val io = new Bundle {
    //val tocode = in Bits (1 bits)
    //val coded = out Bits (2 bits)
    val tocode = slave Flow (Bits(1 bits))
    val coded = master Flow (Bits(2 bits))

  }
  val signals = (0 until 6).map(_ => Bool()).toList ::: List(io.tocode.payload.asBool)
  val trellis1 = trellisGen("171")
  val trellis2 = trellisGen("133")

  //StateMachine
  //  val fsm=new StateMachine{
  //    val IDLE=new State with EntryPoint
  //    val CODING=new State
  //
  //    io.coded.payload(0):=False
  //    io.coded.payload(1):=False
  //    io.coded.valid:=False
  //    signals.dropRight(1).zip(signals.drop(1)).foreach { case (next, prev) => next := RegNext(False)}
  //
  //    IDLE.whenIsActive{
  //      io.coded.payload(0):=False
  //      io.coded.payload(1):=False
  //      io.coded.valid:=False
  //      signals.dropRight(1).zip(signals.drop(1)).foreach { case (next, prev) => next := RegNext(False)}
  //      when(io.tocode.valid===True)(goto(CODING))
  //    }
  //
  //    CODING.onEntry{
  //      io.coded.payload(0) := trellis1.zip(signals).filter { case (bit, reg) => bit == '1' }.map(_._2).reduce(_ ^ _)
  //      io.coded.payload(1):= trellis2.zip(signals).filter { case (bit, reg) => bit == '1' }.map(_._2).reduce(_ ^ _)
  //      io.coded.valid:=True
  //    }
  //      .whenIsActive
  //      {
  //
  io.coded.payload(0) :=False
  io.coded.payload(1) :=False
  signals.dropRight(1).zip(signals.drop(1)).foreach { case (next, prev) => next := RegNext(False)}
  io.coded.valid := False
  when(io.tocode.valid===True){
    (0 until io.tocode.payload.getBitsWidth)
          .foreach { i =>
            io.coded.payload(0) := RegNext(trellis1.zip(signals).filter { case (bit, reg) => bit == '1' }
              .map(_._2).reduce(_ ^ _),init=False)
            io.coded.payload(1) := RegNext(trellis2.zip(signals).filter { case (bit, reg) => bit == '1' }
              .map(_._2).reduce(_ ^ _),init=False)
            //signals(6 to 1):=RegNext(signals.dropRight(1),init=False)
            signals.dropRight(1).zip(signals.drop(1)).foreach { case (next, prev) => next := RegNext(prev, init = False) }
          }
        io.coded.valid := True
 }

    //        when(io.tocode.valid===False)(goto(IDLE))
    //      }}
  //
}






object Convenc{

  def trellisGen(codgen: String) = {
    val out = (0 until codgen.length).map(i => (codgen(i) - '0') * pow(8, codgen.length - 1 - i)).sum
    out.toBinaryString.reverse
  }
  def main(args: Array[String]): Unit = {

    //SpinalVerilog(new convenc)
    //println(trellisGen("133"))
    SimConfig.withWave.compile(new Convenc).doSim{ dut =>
      dut.clockDomain.forkStimulus(period = 10)

      dut.clockDomain.assertReset()
      dut.clockDomain.fallingEdge()
      sleep(10)
      dut.io.tocode.valid #= false
      var idx = 0
      while(idx < 1000){
        val test = Random.nextInt(2)
        dut.io.tocode.payload #= test
//        sleep(10)
//        dut.io.tocode.payload #= 0
//        sleep(10)
//        dut.io.tocode.payload #= 1
//        sleep(10)
//        dut.io.tocode.payload #= 1
//        sleep(10)
//        dut.io.tocode.payload #= 0
        sleep(10)
        idx += 1
        if(idx==100){
          dut.io.tocode.valid #= true


        }
        if(idx==500)
          {
            dut.io.tocode.valid #= false
          }
      }
      sleep(30)

    }
  }


}
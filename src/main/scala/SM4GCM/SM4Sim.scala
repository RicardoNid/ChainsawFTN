package SM4GCM

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw.Real
import Chainsaw._

/**
 * @see [[http://openstd.samr.gov.cn/bzgk/gb/newGbInfo?hcno=7803DE42D3BC5E80B0C3E5D8E873D56A SM4.SM4 standard]]
 * @see [[https://aks.jd.com/tools/sec/ crypto online-judge]]
 */
class SM4Sim extends SM4 with DSPSim[Vec[Bits],Bits,String,String]{
  override val timing:TimingInfo=getTimingInfo

  override def poke(testCase:String,input:Vec[Bits]):Unit={
    input(0) #=BigInt(testCase,16)
    input(1) #=BigInt(testCase,16)
    clockDomain.waitSampling()
  }

  override def peek(output:Bits):String={
    clockDomain.waitSampling()
    output.toBigInt.toString
  }

  override def referenceModel(testCase:String):String="hello"

  override def isValid(refResult:String,dutResult:String):Boolean=true

  override def messageWhenInvalid(testCase:String,refResult:String,dutResult:String):String="hello"

  override def messageWhenValid(testCase:String,refResult:String,dutResult:String):String="hello"
}

object SM4Sim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new SM4Sim).doSim{dut =>
      dut.sim()
      dut.insertTestCase("0123456789ABCDEFFEDCBA9876543210")
      dut.simDone()
    }
  }
}

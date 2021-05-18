package SM4GCM

import Chainsaw._
import spinal.core._
import spinal.core.sim._

/**
 * @see [[http://openstd.samr.gov.cn/bzgk/gb/newGbInfo?hcno=7803DE42D3BC5E80B0C3E5D8E873D56A SM4 standard]]
 * @see [[https://aks.jd.com/tools/sec/ crypto online-judge]]
 */
class SM4Sim extends SM4 with DSPSimTiming[Vec[Bits], Bits, String, String] {

  override def poke(testCase: String, input: Vec[Bits]): Unit = {
    input(0) #= BigInt(testCase, 16)
    input(1) #= BigInt(testCase, 16)
  }

  override def peek(output: Bits): String = {
    output.toBigInt.toString(16)
  }

  override def referenceModel(testCase: String): String = "hello"

  override def isValid(refResult: String, dutResult: String): Boolean = true

  override def messageWhenInvalid(testCase: String, refResult: String, dutResult: String): String =
    s"yours: $dutResult"

  override def messageWhenValid(testCase: String, refResult: String, dutResult: String): String =
    s"yours: $dutResult"
}

object SM4Sim {
  def main(args: Array[String]): Unit = {
    SimConfig.withWave.compile(new SM4Sim).doSim { dut =>
      dut.sim()
      //
      dut.insertTestCase("0123456789ABCDEFFEDCBA9876543210")
      dut.insertTestCase("0123456789ABCDEFFEDCBA9876543210")
      dut.insertTestCase("0123456789ABCDEFFEDCBA9876543210")
      //
      val report = dut.simDone()
      println(report.validLog.mkString("\n"))
    } // ctrl + shift + R
  }
}

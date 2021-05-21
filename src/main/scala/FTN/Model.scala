package FTN

import scala.util.Random

object Model {

  type Bits = Seq[Boolean]

  def main(args: Array[String]): Unit = {
    println(OFDMPositions.mkString(" "))
    println(BitGen(0).mkString(" "))
  }

  def BitGen(frameId:Int) = {
    val rand = new Random(Seed(frameId))
    (0 until BitNumber).map(_ => rand.nextBoolean)
  }

  def Run(frameId:Int) = {}
  def Tx() = {}
  def Rx() = {}

  def OFDMFrameGenerator(bits: Bits) = {

  }

  def Bits2QAM(bits: Bits) = {

  }

  def Convenc(bits: Bits) = {

  }

  // TODO
  //  def GrayQAMCoder
  //  def
}

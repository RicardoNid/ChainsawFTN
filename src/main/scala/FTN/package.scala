import breeze.numerics._
import scala.util.Random

package object FTN {

  // global parameters

  val CPLenth = 20
  val PreambleNumber = 2
  val FFTSize = 512

  // conv-related
  val ConvConstLen = 7
  val ConvCodeGen = Seq(171, 133)
  val tblen = 90
  val ConvCodeRate = 0.5

  val OFDMSymbolNumber = 8
  val BitsPerSymbolQAM = 4
  val PreambleBitsPerSymbolQAM = 4
  val SToPcol = OFDMSymbolNumber / ConvCodeRate

  // caution: matlab indices start from 1 and matlab uses inclusive range
  val DataCarrierPositions = 2 to 225
  val PreambleCarrierPositions = 1 to FFTSize / 2 - 1
  val SubcarriersNum = DataCarrierPositions.length
  val PreambleCarriersNum = PreambleCarrierPositions.length
  val OFDMPositions = Seq(0) ++ DataCarrierPositions ++ Seq(FFTSize / 2) ++ DataCarrierPositions.map(FFTSize - _).reverse

  val BitNumber = DataCarrierPositions.length * OFDMSymbolNumber * BitsPerSymbolQAM
  val PreambleBitNumber = PreambleCarrierPositions.length * BitsPerSymbolQAM

  val Iteration = 5

  val InterleaverDepth = 32

  // complex number needed

  import breeze.math.Complex

  val QAM8Pairs: Seq[(Double, Double)] = Seq((-1 - sqrt(3), 0), (-1, 1), (-1, -1), (0, 1 + sqrt(3)), (0, -1 - sqrt(3)), (1, 1), (1, -1), (1 + sqrt(3), 0)) // real-imag pairs
  val QAM8: Seq[Complex] = QAM8Pairs.map(pair => Complex(pair._1, pair._2))
  val RmsAlloc = Seq(1, sqrt(2), sqrt(3 + sqrt(3)), sqrt(10), sqrt(20), sqrt(42), sqrt(82), sqrt(170))

  val HTap = 20

  // TODO: seed strategy
  val rand = new Random()
  val PreambleSeed = 20
  val FrameNum = 100
  val Seed = (0 until FrameNum).map(_ => rand.nextInt(500))

  // chow-algo related
  val BER = 1e-3
  val SER = 1 - pow((1 - BER), 4)
  // TODO: qfuncinv
  //  val Gap = ???
  val TragetBits = SubcarriersNum * BitsPerSymbolQAM
  val Miu = 1e-5

  // ROMFiles

  def trellisGen(codgen: String) = {
    val out = (0 until codgen.length).map(i => (codgen(i) - '0') * pow(8, codgen.length - 1 - i)).sum
    out.toBinaryString.reverse
  }
}

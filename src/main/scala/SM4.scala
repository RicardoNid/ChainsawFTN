import spinal.core._
import spinal.core.sim._
import spinal.sim._
import spinal.lib._
import spinal.lib.fsm._
import Chainsaw.Real
import Chainsaw._

/** @see [[http://openstd.samr.gov.cn/bzgk/gb/newGbInfo?hcno=7803DE42D3BC5E80B0C3E5D8E873D56A SM4 standard]]
 * @see  [[https://aks.jd.com/tools/sec/ crypto online-judge]]
 */
class SM4 extends Component with Testable {

  // F(X0,X1,X2,X3,rk)=X0⊕T(X1⊕X2⊕X3⊕rk)
  // input =
  def FModule(input: Seq[Bits]) = {
    require(input.length == 5 && input.map(_.getBitsWidth).forall(_ == 32))
    input.head ^ TModule(input.drop(1).reduce(_ ^ _))
  }

  def SBox(input: Bits) = {
    val lut = Array(
      "D690E9FECCE13DB716B614C228FB2C05",
      "2B679A762ABE04C3AA44132649860699",
      "9C4250F491EF987A33540B43EDCFAC62",
      "E4B31CA9C908E89580DF94FA758F3FA6",
      "4707A7FCF37317BA83593C19E6854FA8",
      "686B81B27164DA8BF8EB0F4B70569D35",
      "1E240E5E6358D1A225227C3B01217887",
      "D40046579FD327524C3602E7A0C4C89E",
      "EABF8AD240C738B5A3F7F2CEF96115A1",
      "E0AE5DA49B341A55AD933230F58CB1E3",
      "1DF6E22E8266CA60C02923AB0D534E6F",
      "D5DB3745DEFD8E2F03FF6A726D6C5B51",
      "8D1BAF92BBDDBC7F11D95C411F105AD8",
      "0AC13188A5CD7BBD2D74D012B8E5B4B0",
      "8969974A0C96777E65B9F109C56EC684",
      "18F07DEC3ADC4D2079EE5F3ED7CB3948")
    val lutBits = lut.reduce(_ + _).grouped(2).toSeq.map(hexString => B("h" + hexString.toString))
    require(input.getBitsWidth == 8)
    val asyncROM = Mem(Bits(8 bits), lutBits)
    asyncROM.readAsync(input.asUInt)
  }

  def TModule(input: Bits) = {
    require(input.getBitsWidth == 32)
    // B=(b0,b1,b2,b3)=τ(A)=(Sbox(a0),Sbox(a1),Sbox(a2),Sbox(a3))
    val afterSBox = input.subdivideIn(8 bits).map(SBox).asBits()
    // C=L(B)=B⊕(B<<<2)⊕(B<<<10)⊕(B<<<18)⊕(B<<<24)
    Array(0, 2, 10, 18, 24).map(i => afterSBox.rotateLeft(i)).reduce(_ ^ _)
  }

  def TPrimeModule(input: Bits) = {
    require(input.getBitsWidth == 32)
    // B=(b0,b1,b2,b3)=τ(A)=(Sbox(a0),Sbox(a1),Sbox(a2),Sbox(a3))
    val afterSBox: Bits = input.subdivideIn(8 bits).map(SBox).asBits()
    // C=L(B)=B⊕(B<<<2)⊕(B<<<10)⊕(B<<<18)⊕(B<<<24)
    Array(0, 13, 23).map(i => afterSBox.rotateLeft(i)).reduce(_ ^ _)
  }

  // this part is special in key expansion as FKs are constants
  def initialKeyTransformation(key: Bits) = {
    require(key.getBitsWidth == 128)
    // FK0=(A3B1BAC6),FK1=(56AA3350),FK2=(677D9197),FK3=(B27022DC)
    val FKs = Vec(B"hA3B1BAC6", B"h56AA3350", B"h677D9197", B"hB27022DC").reverse
    // (K0,K1,K2,K3)=(MK0⊕FK0,MK1⊕FK1,MK2⊕FK2,MK3⊕FK3)
    (key ^ FKs.asBits).subdivideIn(32 bits).reverse
  }

  // ki=Ki+4=Ki⊕T'(Ki+1⊕Ki+2⊕Ki+3⊕CKi),i=0,1,...,31
  // input = (Ki, Ki+1, Ki+2, Ki+3, CKi)
  def keyExpansion(input: Seq[Bits]) = {
    require(input.length == 5 && input.map(_.getBitsWidth).forall(_ == 32))
    input.head ^ TPrimeModule(input.drop(1).reduce(_ ^ _))
  }

  val input = slave Flow Vec(Bits(128 bits), 2)
  val plaintext = input.payload(0)
  val secretKey = input.payload(1)
  val counter = Counter(0, 31)

  // cki,j=(4i+j)×7(mod256)
  val CKBytes: Array[Array[Int]] = Array.tabulate(32, 4)(4 * _ + _).map(_.map(_ * 7 % 256))
  val CKs: Array[Bits] = CKBytes.map { bytes =>
    def hex(byte: Int) = "0" * (2 - byte.toHexString.length) + byte.toHexString

    println("h" + bytes.map(hex).reduce(_ + _))
    B("h" + bytes.map(hex).reduce(_ + _))
  }

  val CKRom = Mem(Bits(32 bits), CKs)
  val CK = CKRom.readAsync(counter.value)

  // textSrl(0) is Xi, textSrl(1) is Xi+1 ...
  val textSrl: Seq[Bits] = (0 until 4).map(_ => Reg(Bits(32 bits)))
  val keySrl: Seq[Bits] = (0 until 4).map(_ => Reg(Bits(32 bits)))

  val keyCombined = (keySrl.drop(1) :+ CK).reduce(_ ^ _)
  val keyAfterT = TPrimeModule(keyCombined)
  val keyNext: Bits = keySrl(0) ^ keyAfterT

  val textNext: Bits = FModule(textSrl :+ keyNext)

  val output = master Flow Bits(128 bits)
  output.payload := textSrl.reverse.asBits()
  output.valid := False

  val fsm = new StateMachine {
    val IDLE = StateEntryPoint()
    val INIT = State()
    val RUN = new StateDelay(32)

    IDLE
      .whenIsActive {
        when(input.valid)(goto(INIT))
      }

    INIT
      .whenIsActive {
        goto(RUN)
        (0 until 4).foreach(i => keySrl(i) := initialKeyTransformation(secretKey)(i))
        (0 until 4).foreach(i => textSrl(i) := plaintext.subdivideIn(4 slices).reverse(i))
      }

    RUN
      .whenCompleted {
        when(input.valid)(goto(INIT))
          .otherwise(goto(IDLE))
        output.valid := True
      }
      .whenIsActive {
        counter.increment()
        keySrl.last := keyNext
        keySrl.dropRight(1).zip(keySrl.drop(1)).foreach { case (prev, next) => prev := next }
        textSrl.last := textNext
        textSrl.dropRight(1).zip(textSrl.drop(1)).foreach { case (prev, next) => prev := next }
      }
  }
  override val getTimingInfo: TimingInfo = TimingInfo(1, 1, 33, 33)
}
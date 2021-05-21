package FTN

object ROMs {

  val bitAlloc = Array.fill(SubcarriersNum)(BitsPerSymbolQAM)
  val powerAlloc = Array.fill(SubcarriersNum)(1)

  // TODO: implementHere
  //  val bitAllocAfterChow =
  //  val powerAllocAfterChow

  def main(args: Array[String]): Unit = {
    println(bitAlloc.mkString(" "))
  }

}

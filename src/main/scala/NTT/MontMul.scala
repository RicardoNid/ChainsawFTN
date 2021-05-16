import spinal.core._

import scala.language.postfixOps

//蒙哥马利乘法器函数

object MontMul {
  def apply(A: Bits /*输入的第一个数据*/ ,
            _B: Bits /*输入的第二个数据*/ ,
            N: BigInt /*被模常数*/):
  WallaceTree /*输出的部分化简后的WallaceTree*/ = {
    val n = log2Up(N) //A与B的位宽
    val N_add_1_div_2 = (N + 1) >> 1
    val my_tree = WallaceTree()
    val isDiv2 = Bits(n bits).noCombLoopCheck
    for (i <- 0 until n) {
      my_tree.insert(A & B(n bits, default -> _B(i)), i).insert(N_add_1_div_2, i + 1, isDiv2(i))
    }
    isDiv2 := my_tree.part().full(0, n).conv_Bits(0, n)
    my_tree.subWallaceTree(n).part()
  }
}
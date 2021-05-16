import spinal.core._
import Adder._

import scala.language.postfixOps

object WallaceTree {
  def apply() = new WallaceTree
}

class WallaceTree extends Copyable[WallaceTree] {
  private val Tree = Chain[Chain[WallaceNode]] //WallaceTree
  private val newTree = Chain[WallaceNode] //WallaceTree的新枝

  private def extend(): Unit = { //将树延长一格
    Tree.add(Chain[WallaceNode])
    Tree(Tree.length - 1).add(newTree.copy())
  }

  private def extend(n: Int): WallaceTree = { //将树延长到一定长度
    for (_ <- Tree.length until n) {
      extend()
    }
    this
  }

  def insert(input: Bits, offset: Int): WallaceTree = insert(input, offset, null)

  def insert(input: Bits /*插入的信号*/ ,
             offset: Int /*插入偏移量*/ ,
             byd: Bool /*超出部分的信号，用于处理负数，null表示False*/):
  WallaceTree = { //插入信号
    val w = offset + input.getWidth
    extend(w)
    for (i <- offset until w) {
      Tree(i).add(WallaceNode(input(i - offset)))
    }
    if (byd != null) {
      for (i <- w until Tree.length) {
        Tree(i).add(WallaceNode(byd))
      }
      newTree.add(WallaceNode(byd))
    }
    this
  }

  def insert(input: BigInt, offset: Int): WallaceTree = insert(input, offset, null)

  def insert(input: BigInt /*插入的数字*/ ,
             offset: Int /*插入偏移量*/ ,
             ctrl: Bool /*控制信号，null代表没有*/):
  WallaceTree = { //插入常数
    val isNeg = input < 0
    val w = if (isNeg) offset + log2Up(-input) else offset + log2Up(input + 1)
    var temp: BigInt = 1
    extend(w)
    if (ctrl == null) {
      for (i <- offset until w) {
        if ((input & temp) == temp) {
          Tree(i).add(WallaceNode(True))
        }
        temp = temp << 1
      }
      if (isNeg) {
        for (i <- w until Tree.length) {
          Tree(i).add(WallaceNode(True))
        }
        newTree.add(WallaceNode(True))
      }
    } else {
      for (i <- offset until w) {
        if ((input & temp) == temp) {
          Tree(i).add(WallaceNode(ctrl))
        }
        temp = temp << 1
      }
      if (isNeg) {
        for (i <- w until Tree.length) {
          Tree(i).add(WallaceNode(ctrl))
        }
        newTree.add(WallaceNode(ctrl))
      }
    }
    this
  }

  def insert(input: Bool /*插入的信号*/ ,
             offset: Int /*插入偏移量*/):
  WallaceTree = { //插入单个信号
    extend(offset + 1)
    Tree(offset).add(WallaceNode(input))
    this
  }

  def insert_byd(byd: Bool /*插入的信号*/ ,
                 offset: Int /*插入偏移量*/):
  WallaceTree = { //插入超出部分
    extend(offset)
    for (i <- offset until Tree.length) {
      Tree(i).add(WallaceNode(byd))
    }
    newTree.add(WallaceNode(byd))
    this
  }

  private def simplify(input: Chain[WallaceNode]): Chain[WallaceNode] = { //输入低位输出高位
    val temp = Chain[WallaceNode]
    val output = Chain[WallaceNode]
    while (input.length > 2) {
      temp.replace(input)
      input.clear()
      val w = temp.length
      val times = w / 3
      for (i <- 0 until times) {
        val (s_out, c_out) = Full(temp(3 * i), temp(3 * i + 1), temp(3 * i + 2))
        output.add(c_out)
        input.add(s_out)
      }
      input.add(temp.subChain(3 * times))
    }
    output
  }

  private def byd_simplify(): Unit = { //化简超出位
    while (newTree.length > 2) {
      val high = simplify(newTree)
      extend()
      newTree.add(high)

    }
    if (newTree.length == 2) {
      val high = newTree(0) | newTree(1)
      newTree.replace(newTree(0) ^ newTree(1))
      extend()
      newTree.replace(high)
    }
  }

  private def part_simplify(n: Int /*输入的列*/): Unit = { //部分化简用的函数，将每一位化简为0、1或2位
    Tree(n + 1).add(simplify(Tree(n)))
  }

  private def full_simplify(n: Int): Unit = {
    if (Tree(n).length == 3) {
      val (s_out, c_out) = Full(Tree(n)(0), Tree(n)(1), Tree(n)(2))
      Tree(n).replace(s_out)
      Tree(n + 1).add(c_out)
    } else if (Tree(n).length == 2) {
      val (s_out, c_out) = Half(Tree(n)(0), Tree(n)(1))
      Tree(n).replace(s_out)
      Tree(n + 1).add(c_out)
    }
  }

  def part(): WallaceTree = { //将整个树部分化简
    byd_simplify()
    for (i <- 0 until Tree.length - 1) {
      part_simplify(i)
    }
    if (Tree.length > 0) {
      while (Tree(Tree.length - 1).length > 2) {
        extend()
        part_simplify(Tree.length - 2)
      }
    }
    this
  }

  def full(): WallaceTree = { //将整个树完全化简，要先部分化简
    for (i <- 0 until Tree.length - 1) {
      full_simplify(i)
    }
    if (Tree.length > 0) {
      if (Tree(Tree.length - 1).length > 1) {
        extend()
        full_simplify(Tree.length - 2)
      }
      if (newTree.length > 0) {
        while (Tree(Tree.length - 1).length > 2) {
          extend()
          full_simplify(Tree.length - 2)
        }
        if (Tree(Tree.length - 1).length == 2) {
          newTree.replace(Tree(Tree.length - 1)(0) & !Tree(Tree.length - 1)(1))
          Tree(Tree.length - 1).replace(Tree(Tree.length - 1)(0) ^ Tree(Tree.length - 1)(1))
        }
      } else {
        while (Tree(Tree.length - 1).length > 1) {
          extend()
          full_simplify(Tree.length - 2)
        }
      }
    }
    this
  }

  def full(Start: Int, Count: Int): WallaceTree = { //部分树完全化简，要先部分化简，Count为-1表示彻底的完全化简
    val end = Start + Count
    extend(end + 1)
    for (i <- Start until end) {
      full_simplify(i)
    }
    this
  }

  def conv_Bits(Start: Int = 0, Count: Int): Bits = { //Tree的化简结果以Bits的形式输出
    val end = Start + Count
    val output = Bits(Count bits)
    for (i <- Start until end) {
      if (i >= Tree.length) {
        if (newTree.length == 0) {
          output(i - Start) := False
        } else {
          output(i - Start) := newTree(0)()
        }
      } else {
        if (Tree(i).length == 0) {
          output(i - Start) := False
        } else {
          output(i - Start) := Tree(i)(0)()
        }
      }
    }
    output
  }

  override def copy(): WallaceTree = {
    val output = new WallaceTree
    output.Tree.add(Tree.copy())
    output.newTree.add(newTree.copy())
    output
  }

  def subWallaceTree(n: Int): WallaceTree = { //截掉WallaceTree的前n个枝
    Tree.replace(Tree.subChain(n))
    this
  }

  def add(input: WallaceTree): WallaceTree = { //拼接两个WallaceTree以达到将两个系统相加的效果
    //使用这个函数之前要先对两个WallaceTree进行部分化简
    if (input != null) {
      extend(input.Tree.length)
      for (i <- 0 until input.Tree.length) {
        Tree(i).add(input.Tree(i).copy())
      }
      for (i <- input.Tree.length until Tree.length) {
        Tree(i).add(input.newTree.copy())
      }
      newTree.add(input.newTree.copy())
    }
    this
  }

  def sub(input: WallaceTree): WallaceTree = { //拼接两个WallaceTree以达到将两个系统相减的效果
    //使用这个函数之前要先对两个WallaceTree进行部分化简
    if (input != null) {
      extend(input.Tree.length)
      var idx = 0
      while (idx < input.Tree.length && input.Tree(idx).length < 2) {
        idx = idx + 1
      }
      val is2num = idx != input.Tree.length
      if (is2num) {
        for (i <- 0 until input.Tree.length) {
          if (input.Tree(i).length == 2) {
            Tree(i).add(!input.Tree(i)(0))
            Tree(i).add(!input.Tree(i)(1))
          } else if (input.Tree(i).length == 1) {
            Tree(i).add(!input.Tree(i)(0))
            Tree(i).add(WallaceNode(True))
          } else {
            Tree(i).add(WallaceNode(True))
            Tree(i).add(WallaceNode(True))
          }
        }
        if (input.newTree.length == 1) {
          for (i <- input.Tree.length until Tree.length) {
            Tree(i).add(!input.newTree(0))
            Tree(i).add(WallaceNode(True))
          }
          newTree.add(!input.newTree(0))
          newTree.add(WallaceNode(True))
        } else {
          for (i <- input.Tree.length until Tree.length) {
            Tree(i).add(WallaceNode(True))
            Tree(i).add(WallaceNode(True))
          }
          newTree.add(WallaceNode(True))
          newTree.add(WallaceNode(True))
        }
        extend(2)
        Tree(1).add(WallaceNode(True))
      } else {
        for (i <- 0 until input.Tree.length) {
          if (input.Tree(i).length == 1) {
            Tree(i).add(!input.Tree(i)(0))
          } else {
            Tree(i).add(WallaceNode(True))
          }
        }
        if (input.newTree.length == 1) {
          for (i <- input.Tree.length until Tree.length) {
            Tree(i).add(!input.newTree(0))
          }
          newTree.add(!input.newTree(0))
        } else {
          for (i <- input.Tree.length until Tree.length) {
            Tree(i).add(WallaceNode(True))
          }
          newTree.add(WallaceNode(True))
        }
        extend(1)
        Tree(0).add(WallaceNode(True))
      }
    }
    this
  }
}
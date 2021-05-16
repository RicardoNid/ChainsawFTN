object Chain {
  def apply[T <: Copyable[T]] = new Chain[T]
}

class Chain[T <: Copyable[T]] extends Copyable[Chain[T]] {

  private val Head = Node[T] //头结点
  private var Tail: Node[T] = Head //尾节点
  private var Len = 0 //链表长度
  private var Now: Node[T] = Head //现节点
  private var Pos = -1 //现节点位置

  private def clear_Now(): Unit = { //重置现节点
    Now = Head
    Pos = -1
  }

  private def find(n: Int): Node[T] = { //找到某个节点
    if (n < -1 || n >= Len) {
      throw new IndexOutOfBoundsException(n.toString)
    }
    if (Pos > n) {
      clear_Now()
    }
    for (_ <- Pos until n) {
      Now = Now.get_Next
    }
    Pos = n
    Now
  }

  def apply(n: Int): T = { //获取某个元素
    find(n).get_Data
  }

  def subChain(n: Int): Chain[T] = { //截取链表
    val output = new Chain[T]
    val temp = find(n - 1)
    output.Head.set_Next(temp.get_Next)
    output.Len = Len - n
    if (n != Len) {
      output.Tail = Tail
      Tail = temp
      Tail.del_Next()
      Len = n
    }
    output
  }

  def clear(): Chain[T] = { //清空链表
    Tail = Head.del_Next()
    Len = 0
    clear_Now()
    this
  }

  def add(input: T): Chain[T] = { //在最后方插入元素，不拷贝
    Tail = Tail.set_Next(Node[T]).set_Data(input)
    Len = Len + 1
    this
  }

  def add(input: Chain[T]): Chain[T] = { //在最后方插入链表，不拷贝
    Tail.set_Next(input.Head.get_Next)
    Len = Len + input.Len
    if (input.length != 0) {
      Tail = input.Tail
    }
    this
  }

  def replace(input: T): Chain[T] = { //替换为一个节点
    Tail = Node[T].set_Data(input)
    Head.set_Next(Tail)
    Len = 1
    clear_Now()
    this
  }

  def replace(input: Chain[T]): Chain[T] = { //替换链表
    Head.set_Next(input.Head.get_Next)
    Len = input.Len
    if (Len != 0) {
      Tail = input.Tail
    } else {
      Tail = Head
    }
    clear_Now()
    this
  }

  override def copy(): Chain[T] = { //拷贝链表
    val output = new Chain[T]
    var temp = Head.get_Next
    for (_ <- 0 until Len) {
      output.Tail = output.Tail.set_Next(temp.copy())
      temp = temp.get_Next
    }
    output.Len = Len
    output
  }

  def length: Int = { //获取长度
    Len
  }
}
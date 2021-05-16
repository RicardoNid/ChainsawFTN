object Node {
  def apply[T <: Copyable[T]] = new Node[T]
}

class Node[T <: Copyable[T]] extends Copyable[Node[T]] { //拥有单向指针的节点
  private var Data: T = _
  private var Next: Node[T] = _

  def set_Data(_Data: T): Node[T] = { //赋值，不拷贝
    Data = _Data
    this
  }

  def set_Next(_Next: Node[T]): Node[T] = { //赋次节点
    Next = _Next
    _Next
  }

  def del_Next(): Node[T] = { //删除后方所有节点
    Next = null
    this
  }

  def get_Data: T = { //得到数据
    Data
  }

  def get_Next: Node[T] = { //得到次节点
    Next
  }

  override def copy(): Node[T] = { //对于Node而言是深拷贝，只拷贝数据
    val output = new Node[T]
    output.set_Data(Data.copy())
    output
  }
}
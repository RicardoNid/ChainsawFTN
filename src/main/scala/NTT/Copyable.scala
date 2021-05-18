package NTT

trait Copyable[T] {
  def copy(): T
}
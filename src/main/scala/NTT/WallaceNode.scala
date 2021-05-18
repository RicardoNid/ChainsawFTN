package NTT

import spinal.core._

object WallaceNode {
  def apply(input: Bool) = new WallaceNode(input)
}

class WallaceNode(_Signal: Bool) extends Copyable[WallaceNode] {
  private val Signal: Bool = Bool() //信号

  Signal := _Signal

  def apply(): Bool = Signal

  override def copy(): WallaceNode = {
    val new_Signal = Bool()
    new_Signal := Signal
    new WallaceNode(new_Signal)
  }

  def &(input: WallaceNode): WallaceNode = {
    new WallaceNode(Signal & input.Signal)
  }

  def |(input: WallaceNode): WallaceNode = {
    new WallaceNode(Signal | input.Signal)
  }

  def unary_! : WallaceNode = {
    new WallaceNode(!Signal)
  }

  def ^(input: WallaceNode): WallaceNode = {
    new WallaceNode(Signal ^ input.Signal)
  }
}
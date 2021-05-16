

object Adder { //输入为WallaceNode的全加器和半加器
  def Full(a_in: WallaceNode, b_in: WallaceNode, c_in: WallaceNode):
  (WallaceNode /*s_out*/ ,
    WallaceNode /*c_out*/ ) = { //全加器
    (a_in ^ b_in ^ c_in, ((a_in ^ b_in) & c_in) | (a_in & b_in))
  }

  def Half(a_in: WallaceNode, b_in: WallaceNode):
  (WallaceNode /*s_out*/ ,
    WallaceNode /*c_out*/ ) = { //半加器
    (a_in ^ b_in, a_in & b_in)
  }
}
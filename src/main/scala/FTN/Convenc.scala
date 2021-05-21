package FTN

import spinal.core._
import spinal.core.sim._
import spinal.lib._
import spinal.sim._
import spinal.lib.fsm._

import Chainsaw._
import Chainsaw.Real


/** Config of conv encoding
 *
 * @param length            constraint length
 * @param gens              generator polynomial, in octal form
 * @param codeRate          in the form n/m
 * @param puncturedCodeRate in the form n/m
 */
case class ConvencConfig(length: Int, gens: Seq[Int], codeRate: (Int, Int), puncturedCodeRate: (Int, Int))

class Convenc(config: ConvencConfig) extends ImplicitArea[Vec[Bool]] {

  import config._

  override def implicitValue: Vec[Bool] = ???

}

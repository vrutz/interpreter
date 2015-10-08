package interpreter

/**
 * Created by rutz on 08/10/15.
 */

import scala.meta._

sealed trait Slot
final case class Local(name: Term.Name) extends Slot
case object This extends Slot
case object Super extends Slot
package interpreter

/**
 * Created by rutz on 08/10/15.
 */

import scala.meta._

sealed trait Slot
final case class Local(name: Term.Name) extends Slot
// TODO Find something meaningful as a parameter of This(...)
case object This extends Slot
case object Super extends Slot
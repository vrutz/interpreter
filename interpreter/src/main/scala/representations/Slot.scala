package representations

/**
 * Created by rutz on 08/10/15.
 */

import scala.meta._

sealed trait Slot

case object This extends Slot
case object Super extends Slot

sealed trait Name extends Slot

case object WildCard extends Name
final case class Local(name: Term.Name) extends Name
final case class Tuple(members: Name*) extends Name

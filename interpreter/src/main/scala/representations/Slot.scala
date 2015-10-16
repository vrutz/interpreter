package representations

/**
 * Created by rutz on 08/10/15.
 */

import scala.meta._
import scala.meta.internal.equality.{Typechecking => t}

sealed trait Slot

case object This extends Slot
case object Super extends Slot

sealed trait Name extends Slot

case object WildCard extends Name
final case class Local(name: Term.Name)(implicit c: Context) extends Name {
  override def equals(other: Any): Boolean = other match {
    case Local(otherName) => t.equals(otherName, name)
    case _ => false
  }

  override def hashCode(): Int = t.hashCode(this)
}

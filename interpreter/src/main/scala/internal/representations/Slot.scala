package scala.meta
package internal
package representations

/**
 * Created by rutz on 08/10/15.
 */

import scala.meta.internal.equality.{Semantic => s}

sealed trait Slot

case object This extends Slot
case object Super extends Slot
case object MainFun extends Slot
case object MainArgs extends Slot

sealed trait Name extends Slot

case object Anonymous extends Name
final case class Local(name: Term.Name) extends Name {
  override def equals(other: Any): Boolean = other match {
    case Local(otherName) => s.equals(otherName, name)
    case _ => false
  }

  override def hashCode(): Int = s.hashCode(this.name)
}
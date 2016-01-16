package scala.meta
package internal
package representations

/**
 * Created by rutz on 08/10/15.
 */

import scala.meta.internal.equality.{Semantic => s}

sealed trait Slot

sealed trait Name extends Slot

final case class Local(name: Term.Name) extends Name {
  override def toString: String = name.toString
  
  override def equals(other: Any): Boolean = other match {
    case Local(otherName) => s.equals(otherName, name)
    case _ => false
  }

  override def hashCode(): Int = s.hashCode(this.name)
}
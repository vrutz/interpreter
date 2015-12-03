package scala.meta
package interpreter

import internal.interpreter.Environment
import internal.representations._

/**
 * Created by rutz on 05/10/15.
 */

trait Env {
  def +(name: String, value: Any): Env
}

class EnvImpl(val slots: Map[String, Any] = Map[String, Any]()) extends Env {
  def this(e: Environment) = this(e.get.flatMap {
      case (Local(name), Val(l)) => List(name.toString -> l)
      case _ => Nil
    }.toMap[String, Any])
  def +(name: String, value: Any): Env = new EnvImpl(slots + (name -> value))

  def apply(name: String): Any = slots(name)

  override def toString = s"""Environment(${slots.mkString(", ")})"""
}

object Env {
  def apply(): EnvImpl = new EnvImpl()
  def apply(keyval: (String, Any)*): EnvImpl = new EnvImpl(keyval.toMap)
}
package scala.meta
package internal
package interpreter

import scala.meta.interpreter.{Env, EnvImpl}
import scala.meta.internal.representations._

/**
 * Created by rutz on 03/11/15.
 */


final class Environment(stack: CallStack) extends Env {
  def this() = this(List[Frame](Map[Slot, Value]()))
  def this(e: EnvImpl) = this(List[Frame](e.slots.map {
      case (name: Term.Name, value: Any) => Local(name) -> Literal(value)
    }.toMap))

  // Inherited from interpreter.Env
  def +(name: Term.Name, value: Any): Environment = this + (Local(name), Literal(value))

  // Usable only in Environment
  def apply(name: Slot): Value = stack.head(name)
  def +(name: Slot, value: Value): Environment = new Environment((stack.head + (name -> value)) :: stack.tail)
  def push(frame: Frame): Environment = new Environment(frame :: stack)
  def pop: (Frame, Environment) = (stack.head, new Environment(stack.tail))
  def get: Frame = stack.head

  override def toString = s"""Env(${this.get.mkString("\n")})"""
}
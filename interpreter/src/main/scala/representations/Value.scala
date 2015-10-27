package scala.meta
package representations

import scala.meta._
/**
 * Created by rutz on 05/10/15.
 */
sealed trait Value {
  override def toString: String = this match {
    case Symbol(name) => s"$name"
    case Instance(tpe, fields) => s"$tpe"
    case l: Literal => l.toString
    case Function(name, args, code) => s"def $name($args) = $code"
  }
}

final case class Instance(tpe: Type, fields: Map[Slot, Value]) extends Value

final case class Function(name: Term.Name, args: Seq[Term.Param], code: Term) extends Value

final case class Symbol(name: String) extends Value

final case class Literal(value: Any) extends Value {
  override def toString = value match {
    case null => "null"
    case c: Char => s"\'$c\'"
    case s: Predef.String => "\"" + s + "\""
    case v => v.toString
  }
}

package scala.meta
package internal
package representations

/**
 * Created by rutz on 05/10/15.
 */
sealed trait Value {
  override def toString: String = this match {
    // case Instance(array: Array[_]) => array.toList.toString
    // case Instance(jvmInstance) => jvmInstance.toString
    case l: Literal => l.toString
    case Function(name, args, code) => s"def $name($args) = $code"
    case Main(args, code) => ""// s"def main($args) = $code"
  }
}

// final case class Instance(jvmInstance: Any) extends Value

final case class Function(name: Term.Name, params: Seq[Term.Param], code: Term) extends Value
final case class Main(args: Value, code: Term) extends Value

final case class Literal(value: Any) extends Value {
  override def toString = value match {
    case null => "null"
    case c: Char => s"\'$c\'"
    case s: Predef.String => "\"" + s + "\""
    case a: Array[_] => s"""Array[${a.map(Literal(_).toString).mkString(", ")}]"""
    case v => v.toString
  }
}
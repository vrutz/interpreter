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
    case l: Val => l.toString
    case Function(None, args, code) => s"(${args.mkString(", ")}) => $code"
    case Function(name, args, code) => s"def $name(${args.mkString(", ")}) = $code"
  }
}

// final case class Instance(jvmInstance: Any) extends Value

final case class Function(name: Option[Term.Name], params: Seq[Term.Param], code: Term) extends Value

final case class Val(value: Any) extends Value {
  override def toString = value match {
    case null => "null"
    case c: Char => s"\'$c\'"
    case s: Predef.String => "\"" + s + "\""
    case a: Array[_] => s"""Array[${a.map(Val(_).toString).mkString(", ")}]"""
    case v => v.toString
  }
}
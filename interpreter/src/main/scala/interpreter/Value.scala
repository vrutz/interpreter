package interpreter

/**
 * Created by rutz on 05/10/15.
 */
sealed trait Value {
  override def toString = this match {
    case Symbol(name) => s"$name"
  }
}

final case class Symbol(name: String) extends Value

final case class Literal(value: Any) extends Value {
  override def toString = value match {
    case null => "null"
    case c: Char => s"\'$c\'"
    case s: String => "\"" + s + "\""
    case v => v.toString
  }
}


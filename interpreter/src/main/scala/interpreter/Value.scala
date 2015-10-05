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

sealed abstract class Literal[T](value: T) extends Value {
  override def toString = if(value == null) "null" else value.toString
}

final case class BooleanLit(value: Boolean) extends Literal[Boolean](value)
final case class CharLit(value: Char) extends Literal(value) {
  override def toString = s"\'$value\'"
}
final case class ByteLit(value: Byte) extends Literal(value)
final case class ShortLit(value: Short) extends Literal(value)
final case class IntLit(value: Int) extends Literal(value)
final case class LongLit(value: Long) extends Literal(value)
final case class FloatLit(value: Float) extends Literal(value)
final case class DoubleLit(value: Double) extends Literal(value)
final case class StringLit(value: String) extends Literal(value) {
  override def toString = "\"" + value + "\""
}
case object NullLit extends Literal(null)
case object UnitLit extends Literal(())


package interpreter

/**
 * Created by rutz on 05/10/15.
 */
sealed trait Value

sealed abstract class Literal extends Value {
  override def toString = this match {
    case BooleanLit(v) => s"$v"
    case CharLit(v) => s"\'$v\'"
    case ByteLit(v) => s"$v"
    case ShortLit(v) => s"$v"
    case IntLit(v) => s"$v"
    case LongLit(v) => s"$v"
    case DoubleLit(v) => s"$v"
    case FloatLit(v) => s"$v"
  }
}

final case class BooleanLit(value: Boolean) extends Literal
final case class CharLit(value: Char) extends Literal
final case class ByteLit(value: Byte) extends Literal
final case class ShortLit(value: Short) extends Literal
final case class IntLit(value: Int) extends Literal
final case class LongLit(value: Long) extends Literal
final case class DoubleLit(value: Double) extends Literal
final case class FloatLit(value: Float) extends Literal

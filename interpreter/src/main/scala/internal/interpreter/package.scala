package scala.meta
package internal

import representations._
import scala.runtime.{BoxesRunTime => brt, ScalaRunTime => srt}
import scala.reflect.NameTransformer.decode

/**
 * Created by rutz on 05/10/15.
 */

package object interpreter {
  type Frame = Map[Slot, Value]
  type CallStack = List[Frame]

  def invokePrimitiveUnaryMethod(name: String)(op: Any): Literal = Literal(decode(name) match {
      case "+" => brt.positive(op)
      case "-" => brt.negate(op)
      case "~" => brt.complement(op)
      case "!" => brt.takeNot(op)
      case "toChar" => brt.toCharacter(op)
      case "toByte" => brt.toByte(op)
      case "toShort" => brt.toShort(op)
      case "toInt" => brt.toInteger(op)
      case "toLong" => brt.toLong(op)
      case "toFloat" => brt.toFloat(op)
      case "toDouble" => brt.toDouble(op)
    })

  def invokePrimitiveBinaryMethod(name: String)(op1: Any, op2: Any): Literal = Literal(decode(name) match {
      case "+" => (op1, op2) match {
          case (s: String, _) => s + op2
          case (_, s: String) => op1 + s
          case _ => brt.add(op1, op2)
        }
      case "-" => brt.subtract(op1, op2)
      case "*" => brt.multiply(op1, op2)
      case "/" => brt.divide(op1, op2)
      case "%" => brt.takeModulo(op1, op2)
      case ">>" => brt.shiftSignedRight(op1, op2)
      case "<<" => brt.shiftSignedLeft(op1, op2)
      case ">>>" => brt.shiftLogicalRight(op1, op2)
      case "&" => brt.takeAnd(op1, op2)
      case "|" => brt.takeOr(op1, op2)
      case "^" => brt.takeXor(op1, op2)
      case "&&" => brt.takeConditionalAnd(op1, op2)
      case "||" => brt.takeConditionalOr(op1, op2)
      case "==" => brt.testEqual(op1, op2)
      case "!=" => brt.testNotEqual(op1, op2)
      case "<" => brt.testLessThan(op1, op2)
      case "<=" => brt.testLessOrEqualThan(op1, op2)
      case ">=" => brt.testGreaterOrEqualThan(op1, op2)
      case ">" => brt.testGreaterThan(op1, op2)
      case "#" => ???
      case ":" => ???
      case "\\" => ???
      case "?" => ???
      case "@" => ???
  })

  def invokeObjectBinaryMethod(name:String)(op1: Any, op2: Any): Literal = Literal(decode(name) match {
    case "==" => srt.inlinedEquals(op1.asInstanceOf[AnyRef], op2.asInstanceOf[AnyRef])
    case "!=" => !srt.inlinedEquals(op1.asInstanceOf[AnyRef], op2.asInstanceOf[AnyRef])
    case "equals" => op1 equals op2
    // case "eq" => op1 eq op2
    // case "ne" => op1 ne op2
    case _ => ???
  })

  def invokeObjectUnaryMethod(name:String)(op: Any): Literal = Literal(decode(name) match {
    case "hashCode" => op.hashCode
    case "toString" => op.toString
    case "##" => srt.hash(op)
    case _ => ???
  })

  def invokeArrayMethod(name: String)(op: AnyRef, args: Any*): Literal = Literal(decode(name) match {
    case "length" => srt.array_length(op)
    case "apply" => srt.array_apply(op, args(0).asInstanceOf[Int])
    case "update" => srt.array_update(op, args(0).asInstanceOf[Int], args(1))
    case "clone" => srt.array_clone(op)
  })
  
}

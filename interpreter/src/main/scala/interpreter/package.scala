package scala.meta
import representations._

/**
 * Created by rutz on 05/10/15.
 */


package object interpreter {
//  type TopLevelObjects = Map[Term.Name, Value]
  type Frame = Map[Slot, Value]
  type CallStack = List[Frame]

  type FunSig = (Any, String, Array[Any])

  val symbolToType = Map(
    "Z" -> Boolean,
    "B" -> Byte,
    "S" -> Short,
    "I" -> Int,
    "J" -> Long,
    "F" -> Float,
    "D" -> Double,
    "C" -> Char)

  val nameToSymbol = Map("add" -> "+",
                          "plus" -> "+",
                          "subtract" -> "-",
                          "minus" -> "-",
                          "multiply" ->  "*",
                          "divide" -> "/",
                          "takeModulo" -> "%",
                          "shiftSignedRight" -> ">>",
                          "testGreaterOrEqualThan" -> ">=",
                          "testGreaterThan" -> ">",
                          "testLessOrEqualThan" -> "<=",
                          "shiftSignedLeft" ->  "<<",
                          "shiftLogicalRight" -> ">>>",
                          "testLessThan" -> "<",
                          "testEqual" -> "==",
                          "testNotEqual" -> "!=",
                          "takeAnd" -> "&",
                          "takeOr" -> "|",
                          "takeXor" -> "^",
                          "takeConditionalAnd" -> "&&",
                          "takeConditionalOr" -> "||")

  val scalaIntrinsic: PartialFunction[String, (Literal, Literal, Environment) => (Value, Environment)] = {
    case "+" => (caller: Literal, arg: Literal, env: Environment) => (caller, arg) match {
      case (Literal(op1: Byte), Literal(op2: Byte)) => (Literal(op1 + op2), env)
      case (Literal(op1: Short), Literal(op2: Short)) => (Literal(op1 + op2), env)
      case (Literal(op1: Int), Literal(op2: Int)) => (Literal(op1 + op2), env)
      case (Literal(op1: Long), Literal(op2: Long)) => (Literal(op1 + op2), env)
      case (Literal(op1: Float), Literal(op2: Float)) => (Literal(op1 + op2), env)
      case (Literal(op1: Double), Literal(op2: Double)) => (Literal(op1 + op2), env)
      case (Literal(op1: String), Literal(op2)) => (Literal(op1 + op2), env)
      case (Literal(op1), Literal(op2: String)) => (Literal(op1 + op2), env)
    }
    case "-" => (caller: Literal, arg: Literal, env: Environment) => (caller, arg) match {
      case (Literal(op1: Byte), Literal(op2: Byte)) => (Literal(op1 - op2), env)
      case (Literal(op1: Short), Literal(op2: Short)) => (Literal(op1 - op2), env)
      case (Literal(op1: Int), Literal(op2: Int)) => (Literal(op1 - op2), env)
      case (Literal(op1: Long), Literal(op2: Long)) => (Literal(op1 - op2), env)
      case (Literal(op1: Float), Literal(op2: Float)) => (Literal(op1 - op2), env)
      case (Literal(op1: Double), Literal(op2: Double)) => (Literal(op1 - op2), env)
    }
  }
}

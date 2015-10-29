package scala.meta
package representations

import scala.util.parsing.combinator._
import scala.runtime.ScalaRunTime._

object JVMSignatureParser extends RegexParsers {
  def id        = "[A-Za-z_$/]+".r ^^ { i => i }
  def void: Parser[Class[_]]      = "V".r ^^^ { ().getClass }
  def bool: Parser[Class[_]]      = "Z".r ^^^ { false.getClass }
  def char: Parser[Class[_]]      = "C".r ^^^ { 'a'.getClass }
  def byte: Parser[Class[_]]      = "B".r ^^^ { val x: Byte = 2; x.getClass }
  def short: Parser[Class[_]]     = "S".r ^^^ { val x: Short = 2; x.getClass }
  def int: Parser[Class[_]]       = "I".r ^^^ { 2.getClass }
  def long: Parser[Class[_]]      = "J".r ^^^ { val x: Long = 2; x.getClass }
  def float: Parser[Class[_]]     = "F".r ^^^ { 2.0f.getClass }
  def double: Parser[Class[_]]    = "D".r ^^^ { 2.0.getClass }
  def reference: Parser[Class[_]] = "L" ~> id <~ ";" ^^ { t => Class.forName(t.replace('/', '.')) } 
  def primType: Parser[Class[_]]  = void | bool | char | byte | short | int | long | float | double | reference
  def array: Parser[Class[_]]     = "[" ~> primType ^^ { t: Class[_] => arrayClass(t) }
  def tpe: Parser[Class[_]]       = (primType | array)
  def signature: Parser[JVMSignature]  = "(" ~> rep(tpe) <~ ")" ~> tpe ^^ { 
    case Nil => JVMSignature(Array())
    case sig: List[Class[_]] => JVMSignature(sig.toArray)
  }
}

final case class JVMSignature(arguments: Array[Class[_ <: Any]]) {
  override def toString: String = "[" + arguments.mkString(", ") + "]"

  override def equals(other: Any): Boolean = other match {
    case JVMSignature(otherArgs: Array[Class[_]]) if arguments.isEmpty && otherArgs.isEmpty => true
    case JVMSignature(otherArgs: Array[Class[_]]) if arguments.size == otherArgs.size =>
      otherArgs.zip(arguments).forall {
        case (c0: Class[_], c1: Class[_]) => c0.getName == c1.getName
      }
    case _ => false
  }
}
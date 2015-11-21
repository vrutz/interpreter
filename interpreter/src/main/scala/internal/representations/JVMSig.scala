package scala.meta
package internal
package representations

import scala.util.parsing.input.CharSequenceReader
import scala.util.parsing.combinator._
import scala.runtime.ScalaRunTime._

object JVMSig extends RegexParsers {
  def id        = "[A-Za-z_$/]+".r ^^ { i => i }
  def void: Parser[Class[_]]      = "V".r ^^^ { classOf[Void] }
  def bool: Parser[Class[_]]      = "Z".r ^^^ { classOf[Boolean] }
  def char: Parser[Class[_]]      = "C".r ^^^ { classOf[Char] }
  def byte: Parser[Class[_]]      = "B".r ^^^ { classOf[Byte] }
  def short: Parser[Class[_]]     = "S".r ^^^ { classOf[Short] }
  def int: Parser[Class[_]]       = "I".r ^^^ { classOf[Int] }
  def long: Parser[Class[_]]      = "J".r ^^^ { classOf[Long] }
  def float: Parser[Class[_]]     = "F".r ^^^ { classOf[Float] }
  def double: Parser[Class[_]]    = "D".r ^^^ { classOf[Double] }
  def reference: Parser[Class[_]] = "L" ~> id <~ ";" ^^ { t => Class.forName(t.replace('/', '.')) } 
  def primType: Parser[Class[_]]  = void | bool | char | byte | short | int | long | float | double | reference
  def array: Parser[Class[_]]     = "[" ~> primType ^^ { t: Class[_] => arrayClass(t) }
  def tpe: Parser[Class[_]]       = (primType | array)
  def signature: Parser[JVMSig]  = "(" ~> rep(tpe) <~ ")" ~> tpe ^^ { 
    case Nil => JVMSig(List())
    case sig: List[Class[_]] => JVMSig(sig)
  }

  implicit val parser = signature

  private[internal] def parsing[T](s: String)(implicit p: Parser[T]): T = {
    // Wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    // We need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s) 
    phraseParser(input) match {
          case Success(t, _)     => t
          case NoSuccess(msg, _) =>
            throw new IllegalArgumentException("Could not parse '" + s + "': " + msg)
        }
    }
}

final case class JVMSig(arguments: List[Class[_ <: Any]]) {
  override def toString: String = "[" + arguments.mkString(", ") + "]"

  // override def equals(other: Any): Boolean = other match {
  //   case JVMSig(otherArgs: List[Class[_]]) if arguments.isEmpty && otherArgs.isEmpty => true
  //   case JVMSig(otherArgs: List[Class[_]]) if arguments.size == otherArgs.size =>
  //     otherArgs.zip(arguments).forall {
  //       case (c0: Class[_], c1: Class[_]) => c0.getName == c1.getName
  //     }
  //   case _ => false
  // }
}
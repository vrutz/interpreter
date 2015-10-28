package scala.meta
package representations

import scala.util.parsing.combinator._

object JVMSignatureParser extends RegexParsers {
  def id        = "[A-Za-z_$/]+".r ^^ { i => i }
  def void      = "V".r ^^^ { "scala.Unit" }
  def bool      = "Z".r ^^^ { "scala.Boolean" }
  def char      = "C".r ^^^ { "scala.Char" }
  def byte      = "B".r ^^^ { "scala.Byte" }
  def short     = "S".r ^^^ { "scala.Short" }
  def int       = "I".r ^^^ { "scala.Int" }
  def long      = "J".r ^^^ { "scala.Long" }
  def float     = "F".r ^^^ { "scala.Float" }
  def double    = "D".r ^^^ { "scala.Double" }
  def reference = "L" ~> id <~ ";" ^^ { _.replace('/', '.') } 
  def tpe       = void | bool | char | byte | short | int | long | float | double | reference
  def array     = "[" ~> tpe ^^ { t => s"scala.Array" }
  def signature = "(" ~> rep(tpe | array) <~ ")" ~> tpe ^^ { 
    case Nil => JVMSignature(Array())
    case sig: List[String] => JVMSignature(sig.toArray)
  }
}

final case class JVMSignature(arguments: Array[String]) {
  def getSignature: Array[Class[_ <: Any]] = arguments.map(Class.forName(_))

  override def toString: String = "[" + arguments.mkString(", ") + "]"

  override def equals(other: Any): Boolean = other match {
    case JVMSignature(otherArgs: Array[String]) if arguments.isEmpty && otherArgs.isEmpty => true
    case JVMSignature(otherArgs: Array[String]) if arguments.size == otherArgs.size =>
      otherArgs.zip(arguments).forall {
        case (s1: String, s2: String) => s1 == s2
      }
    case _ => false
  }
}
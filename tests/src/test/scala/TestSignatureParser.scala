package scala.meta

import internal.representations.JVMSignature
import internal.representations.JVMSignatureParser._
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader
import scala.language.implicitConversions

import scala.runtime.ScalaRunTime._


/**
 * Created by rutz on 08/28/15.
 */
class TestSignatureParser extends FunSuite with Matchers {

  private def parsing[T](s:String)(implicit p:Parser[T]):T = {
    // Wrap the parser in the phrase parse to make sure all input is consumed
    val phraseParser = phrase(p)
    // We need to wrap the string in a reader so our parser can digest it
    val input = new CharSequenceReader(s) 
    phraseParser(input) match {
          case Success(t,_)     => t
          case NoSuccess(msg,_) =>
            throw new IllegalArgumentException("Could not parse '" + s + "': " + msg)
        }
    }

    implicit val parser = signature
    implicit def classOf(name: String): Class[_] = Class.forName(name)

    val void = ().getClass
    val bool =  false.getClass
    val char = 'a'.getClass
    val byte = {val x: Byte = 2; x.getClass}
    val short = {val x: Short = 2; x.getClass}
    val int = 2.getClass
    val long = {val x: Long = 2; x.getClass}
    val float = 2.0f.getClass
    val double = 2.0.getClass
    val objct = Class.forName("java.lang.Object")
    val strng = Class.forName("java.lang.String")


    test("invalid signatures") {
      an [IllegalArgumentException] should be thrownBy parsing("()")
      an [IllegalArgumentException] should be thrownBy parsing("")
      an [IllegalArgumentException] should be thrownBy parsing("(,/.)V")
    }

    test("empty signature") {
      parsing("()V") should equal (JVMSignature(Array()))
    }

    test("primitive signature") {
      parsing("(I)V") should equal (JVMSignature(Array[Class[_]](int)))
    }

    test("multiple primitive signature") {
      parsing("(IJZD)V") should equal (JVMSignature(Array[Class[_]](int, long, bool, double)))
    }

    test("reference signature") {
      parsing("(Ljava/lang/Object;)I") should equal (JVMSignature(Array[Class[_]](objct)))
    }

    test("multiple reference signature") {
      parsing("(Ljava/lang/Object;Ljava/lang/String;)I") should equal (JVMSignature(Array[Class[_]](objct, strng)))
    }

    test("array signature") {
      parsing("([I)Z") should equal (JVMSignature(Array[Class[_]]("[I")))
    }

    test("array return type") {
      parsing("()[D") should equal (JVMSignature(Array[Class[_]]()))
    }

    test("multiple array signature") {
      parsing("([I[Z[Ljava/lang/String;)Ljava/lang/String;") should equal (JVMSignature(Array[Class[_]](arrayClass(int), arrayClass(bool), arrayClass(strng))))
    }

    test("all mixed up") {
      parsing("([ILscala/Predef$;JZ[Ljava/util/List;)Ljava/lang/Object;") should equal (JVMSignature(Array[Class[_]](arrayClass(int), "scala.Predef$", long, bool, arrayClass("java.util.List"))))
    }
}

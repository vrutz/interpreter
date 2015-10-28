package scala.meta

import representations.JVMSignature
import representations.JVMSignatureParser._
import org.scalatest._
import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader


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

    test("invalid signatures") {
      an [IllegalArgumentException] should be thrownBy parsing("()")
      an [IllegalArgumentException] should be thrownBy parsing("")
      an [IllegalArgumentException] should be thrownBy parsing("(,/.)V")
    }

    test("empty signature") {
      parsing("()V") should equal (JVMSignature(Array()))
    }

    test("primitive signature") {
      parsing("(I)V") should equal (JVMSignature(Array[String]("scala.Int")))
    }

    test("multiple primitive signature") {
      parsing("(IJZD)V") should equal (JVMSignature(Array[String]("scala.Int", "scala.Long", "scala.Boolean", "scala.Double")))
    }

    test("reference signature") {
      parsing("(Ljava/lang/Object;)I") should equal (JVMSignature(Array[String]("java.lang.Object")))
    }

    test("multiple reference signature") {
      parsing("(Ljava/lang/Object;Ljava/lang/String;)I") should equal (JVMSignature(Array[String]("java.lang.Object", "java.lang.String")))
    }

    test("array signature") {
      parsing("([I)Z") should equal (JVMSignature(Array[String]("scala.Array")))
    }

    test("multiple array signature") {
      parsing("([I[Z[Ljava/lang/String;)Ljava/lang/String;") should equal (JVMSignature(Array[String]("scala.Array", "scala.Array", "scala.Array")))
    }

    test("all mixed up") {
      parsing("([ILscala/Predef$;JZ[Ljava/util/List;)Ljava/lang/Object;") should equal (JVMSignature(Array[String]("scala.Array", "scala.Predef$", "scala.Long", "scala.Boolean", "scala.Array")))
    }
}

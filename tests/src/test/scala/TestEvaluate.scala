package scala.meta

import org.scalatest.FunSuite

import scala.meta.dialects.Scala211
import interpreter.Interpreter._
import representations.Literal
/**
 * Created by rutz on 06/10/15.
 */
class TestEvaluate extends FunSuite {
  val scalaLibrary = sys.props("sbt.paths.scalalibrary.classes")
  // val classpath = sys.props("sbt.paths.scrutinee.classes")
  // val sourcepath = sys.props("sbt.paths.scrutinee.sources")

  implicit val c: Context = Context(Artifact(scalaLibrary))

  test("literal") {
     assert(eval(q"""{ def x = 2; x }""") match {
        case Literal(2) => true
        case _ => false
     })
  }

  test("simple main with args") {
    assert(eval("""
            |object Test {
            |  def main(args: Array[String]): Unit = {
            |    val x = args.length
            |    val y = x * x
            |    println(y)
            |  }
            |}
        """.stripMargin.parse[Stat], Array[String]("test", "if", "it", "works", "for", "6")) match {
        case Literal(()) => true
        case _ => false
    })
  }
/*
  test("defining functions") {
    eval("""
        |object Test {
        |   def loop(x: Int): Int = {
        |       def helper(x: Int) = true
        |       if (helper(x)) 0
        |       else loop(x - 1)
        |   }
        |   def main(args: Array[String]): Unit = {
        |       loop(42)
        |   }
        |}
        """.stripMargin.parse[Stat])
  }

  test("compiled method calls") {
    eval("""
        |object Test {
        |   def main(args: Array[String]): Unit = {
        |       val x = 2
        |       println(x.toString())
        |       println(x + x)
        |   }
        |}
        """.stripMargin.parse[Stat])
  }
  */
}

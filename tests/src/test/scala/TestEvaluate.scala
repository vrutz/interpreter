package scala.meta

import org.scalatest.FunSuite

import tql._
import interpreter._

import internal.representations.{Literal, MainArgs}

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
    val stat: Stat = """
            |object Test {
            |  def main(args: Array[String]): Unit = {
            |    val x = args.length
            |    args update (0, "Hello")
            |    val y = x * x
            |    println(y)
            |    println(args(0) + x)
            |  }
            |}
        """.stripMargin.parse[Stat]

    val args: Term.Name = (stat.topDownBreak collect {
      case q"..$mods def main(${argsName: Term.Name}: Array[String]): Unit = ${expr: Term}" => argsName
    }).head

    val env = Env(args -> Array[String]("test", "if", "it", "works", "for", "6"))
    assert(evalMain(stat, env) match {
        case Literal(()) => true
        case _ => false
    })
  }

  test("defining functions") {
    eval("""
        |{
        |   def loop(x: Int): Int = {
        |       def helper(x: Int) = true
        |       if (helper(x)) 0
        |       else loop(x - 1)
        |   }
        |   loop(42)
        |}
        """.stripMargin.parse[Term])
  }

  test("compiled method calls") {
    evalMain("""
        |object Test {
        |   def main(args: Array[String]): Unit = {
        |       val x = 2
        |       println(x.toString)
        |       println(x.toDouble)
        |       println(x + x)
        |   }
        |}
        """.stripMargin.parse[Stat])
  }
/*
  test("patterns in declarations") {
    eval("""
        |object Test {
        |   def main(args: Array[String]): Unit = {
        |       val (x, y) = (2, 3)
        |       println(x.toString)
        |       println(y.toDouble)
        |       println(x + y)
        |   }
        |}
        """.stripMargin.parse[Stat])
  }

  test("more complex patterns in declarations") {
    eval("""
        |object Test {
        |   def main(args: Array[String]): Unit = {
        |       val ((List(x), y), z) = ((List(2), 3), 4)
        |       println(x.toString)
        |       println(y.toDouble)
        |       println(x + y + z)
        |   }
        |}
        """.stripMargin.parse[Stat])
  }
  */
}

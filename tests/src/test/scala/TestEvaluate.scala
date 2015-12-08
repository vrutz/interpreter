package scala.meta

import org.scalatest.FunSuite

import tql._
import interpreter._
import internal.interpreter._
import internal.interpreter.Interpreter._

import internal.representations._

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
        case Val(2) => true
        case _ => false
     })
  }

  test("unary selection") {
    assert(eval("""
      |{
      |   val x = Array(2, 3, 4, 5, 6, 7)
      |   x.length
      |}""".stripMargin.parse[Term]) match {
        case Val(6) => true
        case _ => false
      })
  }

    test("infix application") {
    assert(eval("""
      |{
      |   val x = Array(2, 3, 4, 5, 6, 7)
      |   x update (0, 1)
      |   x apply (0)
      |}
      """.stripMargin.parse[Term]) match {
        case Val(1) => true
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
            |    println(args.length)
            |    println((args apply 0) + x)
            |  }
            |}
            """.stripMargin.parse[Stat]

    val env = Env("args" -> Array[String]("test", "if", "it", "works", "for", "6"))
    assert(evalMain(stat, env) match {
        case Val(()) => true
        case _ => false
    })
  }

  test("defining functions") {
    assert(eval("""
        |{
        |   def loop(x: Int): Int = {
        |       def helper(x: Int) = true
        |       if (helper(x)) 0
        |       else loop(x - 1)
        |   }
        |   loop(42)
        |}
        """.stripMargin.parse[Term]) match {
        case Val(0) => true
        case _ => false
      })
  }

  test("compiled method calls") {
    assert(eval("""
        |{
        |   val x = 2
        |   x.toString
        |}
        """.stripMargin.parse[Term]) match {
      case Val("2") => true
      case _ => false
    })

    assert(eval("""
        |{
        |   val x = 2
        |   x.toDouble
        |}
        """.stripMargin.parse[Term]) match {
      case Val(2.0) => true
      case _ => false
    })


    assert(eval("""
        |{
        |   val x = 2
        |   x + x
        |}
        """.stripMargin.parse[Term]) match {
      case Val(4) => true
      case _ => false
    })
  }

  test("Creating a List") {
    eval("""{
      |val x = List.apply(2)
      |}""".stripMargin.parse[Term])
  }

  test("literal int") {
    assert(eval(q"1") match {
      case Val(x) if x.isInstanceOf[Int] && x == 1 => true
      case _ => false
    })
  }

  test("literal double") {
    assert(eval(q"1.0") match {
      case Val(x) if x.isInstanceOf[Double] && x == 1.0 => true
      case _ => false
    })
  }

  test("literal char") {
    assert(eval("\'c\'".parse[Term]) match {
      case Val(x) if x.isInstanceOf[Char] && x == 'c' => true
      case _ => false
    })
  }

  test("literal string") {
    assert(eval("\"hello\"".parse[Term]) match {
      case Val(x) if x.isInstanceOf[Predef.String] && x == "hello" => true
      case _ => false
    })
  }

  test("if true") {
    assert(eval(q"if (true) { 1 } else { 0 }") match {
      case Val(1) => true
      case _ => false
    })
  }

  test("if false") {
    assert(eval(q"if (false) { 1 } else { 0 }") match {
      case Val(0) => true
      case _ => false
    })
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

  def evalMain(stat0: Stat, env: EnvImpl = Env())(implicit ctx: Context): Value = {
    val stat = ctx.typecheck(stat0).asInstanceOf[Stat]
    val internalEnv = Environment(env, stat)(ctx)
    // println(internalEnv)

    stat match {
      // Stat can be either a block (multiple statements like class/object def and imports etc...)
      // Or it is an object containing the main function

      case q"object $name extends $template" =>
        val template"{ ..$_} with ..$_ { $_ => ..$stats1 }" = template

        val (completeEnv, main) = stats1.foldLeft[(Environment, Option[Term.Name])]((internalEnv, None)) {
          case ((tEnv, mainName), q"..$mods def $main(..$params): Unit = ${expr: Term}") if main.toString == "main" =>
          val argsName: Term.Name = params match {
            case Seq(param) => param.name match {
              case name: Term.Name => name
            }
          }
            val args: Val = tEnv.get.getOrElse(Local(argsName), Val(Array[String]())).asInstanceOf[Val]
            (tEnv + (Local(main), Function(main, params, expr)) + (Local(argsName), args), Some(main))
          case ((tEnv, mainName), q"..$mods def $name[..$tparams](..$params): $tpeopt = $expr") => 
            (tEnv + (Local(name), Function(name, params, expr)), mainName)
        }

        val Function(_, arguments, term) = completeEnv(Local(main.get))
        evaluate(term, completeEnv)(ctx)
        Val(())

      case t: Term => evaluate(ctx.typecheck(t).asInstanceOf[Term].desugar(ctx), internalEnv)(ctx)._1
    }
  }
}

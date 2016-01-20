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
  val scalaLib = sys.props("sbt.paths.scalalibrary.classes")
  val scalacp = "/Users/rutz/.ivy2/cache/org.scala-lang/scala-library/jars/scala-library-2.11.7.jar"
  val scalametacp = "/Users/rutz/.ivy2/cache/org.scalameta/scalameta_2.11/jars/scalameta_2.11-0.1.0-SNAPSHOT.jar"

  // implicit val c: Context = Context(Artifact(scalacp), Artifact(scalametacp))
  implicit val c: Context = Context(Artifact(scalaLib))

  test("literal") {
     assert(eval(q"""{ def x = 2; x }""") == Val(2), true)
  }

  test("unary selection") {
    assert(eval("""
      |{
      |   val x = Array(2, 3, 4, 5, 6, 7)
      |   x.length
      |}""".stripMargin.parse[Term]) == Val(6))
  }

    test("infix application") {
    assert(eval("""
      |{
      |   val x = Array(2, 3, 4, 5, 6, 7)
      |   x(0) = 1
      |   x(0)
      |}
      """.stripMargin.parse[Term]) == Val(1))
  }

  test("simple main with args") {
    val stat: Stat = """
            |object Test {
            |  def main(args: Array[String]): Unit = {
            |    val x = args.length
            |    args.update(0, "Hello")
            |    val y = x * x
            |    println(y)
            |    println(args.length)
            |    println(args.apply(0) + x)
            |  }
            |}
            """.stripMargin.parse[Stat]

    val env = Env("args" -> Array[String]("test", "if", "it", "works", "for", "6"))
    assert(evalMain(stat, env)  == Val(()))
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
        """.stripMargin.parse[Term])  == Val(0))
  }

  test("factorial") {
    assert(eval("""
        |{
        |   def factorial(x: Int): Int = {
        |     x match {
        |       case z if z == 0 || z == 1 => 1
        |       case _ => x * factorial(x - 1)
        |     }
        |   }
        |   val y = factorial(3)
        |   println(y)
        |   y
        |}
        """.stripMargin.parse[Term])  == Val(6))
  }

  test("compiled method calls") {
    assert(eval("""
        |{
        |   val x = 2
        |   x.toString
        |}
        """.stripMargin.parse[Term])  == Val("2"))

    assert(eval("""
        |{
        |   val x = 2
        |   x.toDouble
        |}
        """.stripMargin.parse[Term])  == Val(2.0))


    assert(eval("""
        |{
        |   val x = 2
        |   x + x
        |}
        """.stripMargin.parse[Term])  == Val(4))
  }

  test("Creating a List") {
    assert(eval("""
      |{
      |  val x = List(2)
      |  x
      |}
      """.stripMargin.parse[Term]) == Val(List(2)))
  }

    test("Creating a List using apply") {
    assert(eval("""
      |{
      |  val x = List.apply(2)
      |  x
      |}
      """.stripMargin.parse[Term]) == Val(List(2)))
  }

  test("Call methods on List") {
    assert(eval("""{
      |  val x = List(2)
      |  x apply (0)
      |}""".stripMargin.parse[Term]) == Val(2))
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
    assert(eval(q"if (true) { 1 } else { 0 }") == Val(1))
  }

  test("if false") {
    assert(eval(q"if (false) { 1 } else { 0 }") == Val(0))
  }

  test("simple match") {
    assert(eval("""
      |2 match {
      |  case _ => 0
      |}""".stripMargin.parse[Term]) == Val(0))
  }

  test("match with literal") {
    assert(eval("""
      |2 match {
      |  case 2 => 0
      |}""".stripMargin.parse[Term]) == Val(0))
  }

  test("match with renaming") {
    assert(eval("""
      |2 match {
      |  case x => x + 1
      |}""".stripMargin.parse[Term]) == Val(3))
  }

  test("match with comparison to variable") {
    assert(eval("""
      |{
      |val x = 2
      |2 match {
      |  case `x` => x + 1
      |}}""".stripMargin.parse[Term]) == Val(3))
  }

  test("match with if condition") {
    assert(eval("""
      |2 match {
      |  case x if x == 2 => x + 1
      |}""".stripMargin.parse[Term]) == Val(3))
  }

  test("match with multiple matches but early match") {
    assert(eval("""
      |2 match {
      |  case x if x == 2 => x + 1
      |  case x => x + 2
      |  case 3 => 3
      |  case 0 => 1
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case _ => 0
      |}""".stripMargin.parse[Term]) == Val(3))
  }

  test("match with multiple matches but late match") {
    assert(eval("""
      |2 match {
      |  case x if x < 0 => x + 2
      |  case 3 => 3
      |  case 0 => 1
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case y if y == 4 => y + 4
      |  case x if x == 2 => x + 1
      |  case _ => 0
      |}""".stripMargin.parse[Term]) == Val(3))
  }

  test("match with good type ascribtion") {
    assert(eval("""
      |{
      |  2 match {
      |    case x: Int => x + 2
      |    case _ => 0
      |  }
      |}""".stripMargin.parse[Term]) == Val(4))
  }

  test("match with wrong type ascribtion") {
    assert(eval("""
      |{
      |  val x: Any = 2
      |  x match {
      |    case x: Double => x + 2
      |    case _ => 0
      |  }
      |}""".stripMargin.parse[Term]) == Val(0))
  }

  test("compiled call on List") {
    assert(eval("""
      |{
      |  val x = List(2)
      |  x.length
      |}""".stripMargin.parse[Term]) == Val(1))
  }

  test("identity lambda function") {
    assert(eval(q"""{val x = (y: Int) => y; x(2) }""") == Val(2))
  }

  test("more complicated lambda function") {
    assert(eval("""
      |{
      |  val x = (z: Int) => z match {
      |      case 0 => List(0)
      |      case y if y < 10 => List(y, y - 1, y - 2)
      |      case a: Int => List(a, a + 1, z + 2)
      |      case _ => List(-1)  
      |    }
      |  x(8)
      |}""".stripMargin.parse[Term]) == Val(List(8, 7, 6)))
  }

  test("factorial lambda function") {
    assert(eval("""
      |{
      |  val x: Int => Int = (z: Int) => z match {
      |      case 0 => 0
      |      case y if y == 1 => y
      |      case a: Int => a * x(a - 1)
      |  }
      |  x(3)
      |}""".stripMargin.parse[Term]) == Val(6))
  }

  test("assignment variable"){
    assert(eval(q"""{ var x = 1; x = 2; x }""") == Val(2))
  }

  test("assignement variable of instance") {
    assert(eval(q"""{ var x = List(1); x = List(2); x }""") == Val(List(2)))
  }

  test("quicksort reflection") {
    val Val(res: Array[Int]) = eval(q"""{val x = Array(2, 4, 1, 3); scala.util.Sorting.quickSort(x); x}""")
    assert(res.zip(Array(1, 2, 3, 4)) forall {
        case (a, b) => a == b
      })
  }

  // test("compiled quicksort") {
  //   val classpath = sys.props("sbt.paths.scrutinee.classes")
  //   val sourcepath = sys.props("sbt.paths.scrutinee.sources")
  //   implicit val c = Context(Artifact(classpath, sourcepath))
  //   val Val(res: Array[Int]) = eval(q"""{val x = Array(2, 1, 3, 4); O.quicksort(x); x}""")
  //   assert(res.zip(Array(1, 2, 3, 4)) forall {
  //       case (a, b) => a == b
  //     })
  // }

  test("println call") {
    assert(eval(q"""{println("Using println"); ()}""") == Val(()))
  }

  test("example presentation") {
    assert(eval(""" 
      |{ 
      |  val x = Array(0, 1, 2)
      |  x(0) = 2
      |  println(x(1) + x(0))
      |  val y = List(1, 2, 3) 
      |  println(y.head + y.length) 
      |  val z = (a: List[Int]) => a.tail.head + a.length
      |  z(y)
      |} """.stripMargin.parse[Term]) == Val(5), true)
  }

  // test("Macro 1 Scala Days") {
  //   eval("""
  //     |{
  //     |  def adtImpl(T: Type)(implicit c: Context) = {
  //     |    T match {
  //     |      case ref: Type.Ref =>
  //     |        def validateLeaf(leaf: Member) = {
  //     |          if (!leaf.isFinal) abort(s"${leaf.name} is not final")
  //     |          if (!leaf.isCase) abort(s"${leaf.name} is not sealed")
  //     |          if (!leaf.tparams.isEmpty) abort(s"${leaf.name} is not monomorphic")
  //     |        }
  //     |        val defn = ref.defn
  //     |        if (defn.isClass || defn.isObject) {
  //     |          validateLeaf(defn)
  //     |        } else if (defn.isTrait) {
  //     |          if (defn.isSealed) defn.submembers.foreach(validateLeaf)
  //     |          else abort(s"${defn.name} is not sealed")
  //     |        } else {
  //     |          abort(s"unsupported ref to ${defn.name}")
  //     |        }
  //     |        q"new Adt[$T]{}"
  //     |      case _ =>
  //     |        abort(s"unsupported type $T")
  //     |    }
  //     |  }
  //     |}
  //     """.stripMargin.parse[Term])
  // }

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
            (tEnv + (Local(main), Function(Some(main), params, expr)) + (Local(argsName), args), Some(main))
          case ((tEnv, mainName), q"..$mods def $name[..$tparams](..$params): $tpeopt = $expr") => 
            (tEnv + (Local(name), Function(Some(name), params, expr)), mainName)
        }

        val Function(_, arguments, term) = completeEnv(Local(main.get))
        evaluate(term, completeEnv)(ctx)
        Val(())

      case t: Term => evaluate(ctx.typecheck(t).asInstanceOf[Term].desugar(ctx), internalEnv)(ctx)._1
    }
  }
}

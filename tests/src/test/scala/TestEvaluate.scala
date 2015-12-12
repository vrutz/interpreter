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
      |}""".stripMargin.parse[Term]) == Val(6))
  }

    test("infix application") {
    assert(eval("""
      |{
      |   val x = Array(2, 3, 4, 5, 6, 7)
      |   x update (0, 1)
      |   x apply (0)
      |}
      """.stripMargin.parse[Term]) == Val(1))
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

  test("Call methods on Set") {
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

  test("Macro 1 Scala Days") {
    eval("""
      |{
      |  def adtImpl(T: Type)(implicit c: Context) = {
      |    T match {
      |      case ref: Type.Ref =>
      |        def validateLeaf(leaf: Member) = {
      |          if (!leaf.isFinal) abort(s"${leaf.name} is not final")
      |          if (!leaf.isCase) abort(s"${leaf.name} is not sealed")
      |          if (!leaf.tparams.isEmpty) abort(s"${leaf.name} is not monomorphic")
      |        }
      |        val defn = ref.defn
      |        if (defn.isClass || defn.isObject) {
      |          validateLeaf(defn)
      |        } else if (defn.isTrait) {
      |          if (defn.isSealed) defn.submembers.foreach(validateLeaf)
      |          else abort(s"${defn.name} is not sealed")
      |        } else {
      |          abort(s"unsupported ref to ${defn.name}")
      |        }
      |        q"new Adt[$T]{}"
      |      case _ =>
      |        abort(s"unsupported type $T")
      |    }
      |  }
      |}
      """.stripMargin.parse[Term])
  }

  // test("Macro 2 Scala Days") {
  //   eval("""
  //     |{
  //     |  def serializeImpl(T: Type)(implicit c: Context) = {
  //     |    T match {
  //     |      case ref: Type.Ref =>
  //     |        val defn = ref.defn
  //     |        if (defn.isClass || defn.isTrait || defn.isObject) {
  //     |          val serializer = Term.fresh(defn.name + "Serializer")
  //     |          val input = Term.fresh("input")
  //     |          val body = {
  //     |            def serializer(defn: Member, input: Term.Name, tagged: Boolean) = {
  //     |              val fields = defn.ctor.params.map(_.field)
  //     |              var entries: Seq[Term] = fields.map { field =>
  //     |                q""" "\"" + ${field.name.toString} + "\": " + serialize($input.${field.name}) """
  //     |              }
  //     |              if (tagged) {
  //     |                val tag = defn.supermembers.head.submembers.sortBy(_.name.toString).indexOf(defn).toString
  //     |                entries :+= q""" "$$tag: " + $tag """
  //     |              }
  //     |              val unwrappedResult = entries.foldLeft(None: Option[Term]) { (acc, curr) =>
  //     |                acc.map(acc => q"""$acc + ", " + $curr""").orElse(Some(curr))
  //     |              }
  //     |              val contents = unwrappedResult.getOrElse(q""" "" """)
  //     |              q""" "{" + $contents + "}" """
  //     |            }
  //     |            if (defn.isClass) {
  //     |              serializer(defn, input, tagged = false)
  //     |            } else if (defn.isObject) {
  //     |              serializer(defn, input, tagged = false)
  //     |            } else if (defn.isTrait) {
  //     |              val refined = Pat.fresh("input")
  //     |              val clauses = defn.submembers.map(leaf => p"case $refined: ${leaf.tpe.pat} => ${serializer(leaf, refined.name, tagged = true)}")
  //     |              q"$input match { ..case $clauses }"
  //     |            } else {
  //     |              abort(s"unsupported ref to ${defn.name}")
  //     |            }
  //     |          }
  //     |          q"""
  //     |            implicit object $serializer extends Serializer[$T] {
  //     |              def serialize($input: $T): String = $body
  //     |            }
  //     |            $serializer
  //     |          """
  //     |        } else {
  //     |          abort(s"unsupported ref to ${defn.name}")
  //     |        }
  //     |      case _ =>
  //     |        abort(s"unsupported type $T")
  //     |    }
  //     |  }
  //     |}""")
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

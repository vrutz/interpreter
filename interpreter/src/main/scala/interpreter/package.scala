package scala.meta

import scala.meta.internal.interpreter.Interpreter._
import scala.meta.internal.interpreter.Environment
import scala.meta.internal.representations._
import scala.meta.internal.equality.{Semantic => s}

package object interpreter {
  def evalMain(stat0: Stat, env: EnvImpl = Env())(implicit ctx: Context): Value = {
    val stat = ctx.typecheck(stat0).asInstanceOf[Stat]
    val internalEnv = Environment(env, stat)
    // println(internalEnv)

    stat match {
      // Stat can be either a block (multiple statements like class/object def and imports etc...)
      // Or it is an object containing the main function

      case q"object $name extends $template" =>
        val template"{ ..$_} with ..$_ { $_ => ..$stats1 }" = template

        val completeEnv: Environment = stats1.foldLeft(internalEnv) {
          case (tEnv, q"..$mods def main(${argsName: Term.Name}: Array[String]): Unit = ${expr: Term}") =>
            val args = tEnv.get.getOrElse(Local(argsName), Nil)
            tEnv + (MainFun, Main(Literal(args), expr)) + (Local(argsName), Literal(args))
          case (tEnv, q"..$mods def $name[..$tparams](..$params): $tpeopt = $expr") => 
            tEnv + (Local(name), Function(name, params, expr))
        }

        val Main(arguments, term) = completeEnv(MainFun)
        evaluate(term, completeEnv)
        Literal(())

      case t: Term => evaluate(ctx.typecheck(t).asInstanceOf[Term].desugar, internalEnv)._1
    }
  }

  def eval(term0: Term, env: EnvImpl = Env())(implicit ctx: Context): Value = {
    val term: Term = ctx.typecheck(term0).asInstanceOf[Term]
    evaluate(term.desugar, Environment(env, term))._1
  }
}
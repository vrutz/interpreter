package scala
package meta

import scala.meta.internal.interpreter.Interpreter._
import scala.meta.internal.interpreter.Environment
import scala.meta.internal.representations._
import scala.meta.internal.equality.{Semantic => s}

package object interpreter {
  def eval(term0: Term, env: EnvImpl = Env())(implicit ctx: Context): Value = {
    val term: Term = ctx.typecheck(term0).asInstanceOf[Term]
    evaluate(term.desugar, Environment(env, term))._1
  }
}
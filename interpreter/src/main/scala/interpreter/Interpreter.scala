package scala.meta
package interpreter

import representations.Anonymous
import representations._

object Interpreter {

  def evaluate(terms: Seq[Term], env: Environment)(implicit ctx: Context): (Seq[Value], Environment) = {
    terms.foldRight(List[Value](), env) {
      case (expr, (evaluatedExprs, newEnv)) =>
        val (newEvaluatedExpr, envToKeep) = evaluate(expr, newEnv)
        (evaluatedExprs ::: List(newEvaluatedExpr), envToKeep)
    }
  }

  def evaluate(term0: Term, env: Environment = new Environment())(implicit ctx: Context): (Value, Environment) = {
    val term: Term = ctx.typecheck(term0).asInstanceOf[Term]
    term.desugar match {
      /* Literals */
      case x: Lit => (Literal(x.value), env)

      /* Expressions */
      // this
      case q"this" => (env(This), env)

      //  super: super | super[<expr>]
          case q"super" => (env(Super), env)
          case q"super[$_]" => ???
//            val (typeToApp: interpreter.Type, typeEnv) = evaluate(qname, env)
//            (env(Super), typeEnv)

      //    case q"${name: Term.Name}" => (env(Local(name)), env)
      // Selection <expr>.<name>
      // Will cover all $stg.this, $stg.super etc...
      case q"${expr: Term}.${name: Term.Name}" =>
        val (evalExpr: Instance, envExpr) = evaluate(expr, env)
        name.defn match {
          case q"..$mods val ..$pats: $tpeopt = ${expr: Term}" => (evalExpr.fields(Local(name)), envExpr)
          case q"..$mods var ..$pats: $tpeopt = $expropt" if expropt.isDefined => (evalExpr.fields(Local(name)), envExpr)
          case q"..$mods def $name: $tpeopt = ${expr: Term}" => evaluate(expr, envExpr)
          case q"..$mods def $name(): $tpeopt = ${expr: Term}" => evaluate(expr, envExpr)
        }

      // Application <expr>(<aexprs>) == <expr>.apply(<aexprs)
      case q"${expr: Tree}(..$aexprs)" =>
        // Same as infix but with method apply
        evaluate(q"$expr apply (..$aexprs)", env)
      //    case q"$expr[..$tpes]" => ???
      case q"${expr0: Term} ${name: Term.Name} ${expr1: Term.Arg}" =>
        val (caller: Value, callerEnv: Environment) = evaluate(expr0, env)
        val (arg: Value, argEnv: Environment) = evaluate(expr1 match {
          case arg"$name = $expr" => expr
          case arg"$expr: _*" => expr
          case expr: Term => expr
        }, callerEnv)
        name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = ${expr2: Term}" => paramss match {
            case List(param"..$mods ${paramname: Term.Param.Name}: $atpeopt = $expropt") =>
              val name: Slot = paramname match {
                case _: Name.Anonymous => Anonymous
                case paramName: Term.Name => Local(paramName)
                case _ => println("Ohoh"); ???
              }
              val (result: Value, resultEnv: Environment) = evaluate(expr2, argEnv push Map(name -> arg, This -> caller))
              (result, resultEnv.pop._2)
          }
        }
      case q"${expr: Term} ${name: Term.Name} (..$aexprs)" =>
        val (caller, callerEnv) = evaluate(expr, env)
        val paramss: Seq[Term.Param] = name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = $expr" => paramss
        }
        println(paramss)
        // TODO need to be careful with the different ways to use arguments but let's do it like this for now
        ???

      // Unary application: !<expr> | ~<expr> | +<expr> | -<expr>
      case q"!${expr: Term}" =>
        val (evaluatedExpr, exprEnv) = evaluate(expr, env)
        (evaluatedExpr match {
          case Literal(bool: Boolean) => Literal(!bool)
        }, exprEnv)
      case q"~${expr: Term}" =>
        val (evaluatedExpr, exprEnv) = evaluate(expr, env)
        (evaluatedExpr match {
          case Literal(e: Byte) => Literal(~e)
          case Literal(e: Short) => Literal(~e)
          case Literal(e: Int) => Literal(~e)
          case Literal(e: Long) => Literal(~e)
        }, exprEnv)
      case q"+${expr: Term}" =>
        val (evaluatedExpr, exprEnv) = evaluate(expr, env)
        (evaluatedExpr match {
          case Literal(e: Byte) => Literal(+e)
          case Literal(e: Short) => Literal(+e)
          case Literal(e: Int) => Literal(+e)
          case Literal(e: Long) => Literal(+e)
          case Literal(e: Float) => Literal(+e)
          case Literal(e: Double) => Literal(+e)
        }, exprEnv)
      case q"-${expr: Term}" =>
        val (evaluatedExpr, exprEnv) = evaluate(expr, env)
        (evaluatedExpr match {
          case Literal(e: Byte) => Literal(-e)
          case Literal(e: Short) => Literal(-e)
          case Literal(e: Int) => Literal(-e)
          case Literal(e: Long) => Literal(-e)
          case Literal(e: Float) => Literal(-e)
          case Literal(e: Double) => Literal(-e)
        }, exprEnv)
      case q"${ref: Term.Ref} = ${expr: Term}" =>
        val (evaluatedRef, refEnv) = evaluate(ref, env)

        ???

      case q"{ ..$stats}" =>
        val lastFrame: Frame = env.get
        val blockEnv = env push lastFrame
        val (l: List[Value], newEnv: Environment) = stats.foldLeft((List[Value](), blockEnv)) {
          case ((evaluatedExprs, exprEnv), nextExpr) =>
            println(nextExpr)
            nextExpr match {
              // TODO Scala meta bug
              //            case q"${expr: Term}" => evaluate(expr, exprEnv)
              case q"..$mods val ..$pats: $tpeopt = $expr" => (Literal(()) :: evaluatedExprs, link(pats, expr, exprEnv))
              case q"..$mods var ..$pats: $tpeopt = $expropt" => ???
            }
            ???
        }
        (l.head, newEnv)
      case t => (Literal(null), env)
    }
  }

  def link(pats: Seq[Pat], expr: Term, env: Environment)(implicit c: Context): Environment = {
    pats.foldLeft(env) {
      case (newEnv, p"_") => evaluate(expr, newEnv)._2

      // TODO Be careful with top level vs not top level patterns
      // TODO Top level are Pat.Var.Term and not top level are Term.Name
      // TODO Think of the val X = 2; val Y = 3; val (X, Y) = (2, 4) example

//      case q"${name: Term.Name}" => ???
      case m: Member.Term =>
        val (evaluatedExpr: Value, exprEnv) = evaluate(expr, env)
        exprEnv + (Local(m.name), evaluatedExpr)

      case (newEnv, p"(..$pats0)") => expr match {
        case q"(..$exprs)" => (pats0 zip exprs).foldLeft(newEnv) {
          case (newNewEnv: Environment, (pat, e)) => link(Seq(pat), e, newNewEnv)
        }
      }

      case (newEnv, p"$ref(..$apats)") =>
        val justArgExprs = expr match {
          case q"$expr0(..$aexprs)" =>
            aexprs map {
              case arg"$name = $expr0" => expr0
              case arg"$expr0: _*" => expr0
              case arg"$expr0" => expr0
            }
        }
        val justPats = apats map {
          case parg"$pat" => Seq(pat)
//          case parg"_*" => p"_"
        }
        // TODO Scala meta bug
//        (justPats zip justArgExprs).foldLeft(newEnv) {
//          case (newNewEnv: Environment, (seqPat, e: Term)) => link(seqPat, e, newNewEnv)
//        }
        ???
    }
  }

	def find(toFind: String)(tree: Tree): Unit = tree.topDownBreak.collect {
    case t@q"$expr.$name" if (name: Term.Name).toString == toFind =>
      println(s"Found one $toFind in expression: $t")
      find(toFind)(expr)
    case t: Term.Name if t.toString == toFind =>
      println(s"Found one $toFind in expression: $t")
  }
}
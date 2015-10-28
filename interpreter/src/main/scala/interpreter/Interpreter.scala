package scala.meta
package interpreter

import representations.Anonymous
import representations._
import scala.meta.internal.{ast => m}
import scala.meta.internal.ffi.Ffi._

object Interpreter {

  def eval(stat: Stat, args: Term = q"Array[String]()")(implicit ctx: Context): Value = ctx.typecheck(stat) match {
    // Stat can be either a block (multiple statements like class/object def and imports etc...)
    // Or it is an object containing the main function

    case q"object $name extends $template" =>
      val template"{ ..$_ } with ..$_ { $_ => ..$stats1 }" = template
      val env: Environment = stats1.foldLeft(new Environment()) {
        case (env, q"..$mods def main(${argsName: Term.Name}: Array[String]): Unit = ${ expr:Term }") =>
          println("Found Main")
          val evalArgs = evaluate(args)._1
          env + (MainFun, Main(evalArgs, expr)) + (Local(argsName), evalArgs)
        case (env, q"..$mods def $name[..$tparams](..$params): $tpeopt = $expr") => 
          env + (Local(name), Function(name, params, expr))
      }
      // TODO Traverse the whole file to build the environment with all the different functions/fields etc...
      // TODO find main method in an object and evaluate its body with args in the environment
      val Main(arguments, term) = env(MainFun)
      println(term.show[Syntax])
      // Find a way to add the arguments to the environment
      evaluate(term, env)
      Literal(())

    case t: Term => evaluate(t.desugar)._1
    
    case q"{ ..$stats }" => 
      println(s"Term $stat");???

    
    case q"import ..$importersnel" =>
      println("import $stat"); ???
  }

  private def evaluate(terms: Seq[Term], env: Environment)(implicit ctx: Context): (Seq[Value], Environment) = {
    terms.foldRight(List[Value](), env) {
      case (expr, (evaluatedExprs, newEnv)) =>
        val (newEvaluatedExpr, envToKeep) = evaluate(expr, newEnv)
        (evaluatedExprs ::: List(newEvaluatedExpr), envToKeep)
    }
  }

  private def evaluate(term: Term, env: Environment = new Environment())(implicit ctx: Context): (Value, Environment) = {
    term match {
      /* Literals */
      case x: Lit => (Literal(x.value), env)

      /* Expressions */
      // this
      case q"this" => (env(This), env)

      //  super: super | super[<expr>]
      case q"super" => (env(Super), env)
      case q"super[$_]" => (env(Super), env)

      case name: Term.Name => env(Local(name)) match {
         case l @ Literal(value) => (l, env)
         case f @ Function(name, Nil, expr) => evaluate(expr, env)
         case f @ Function(name, args, expr) => ???
        }

    // Selection <expr>.<name>
    // Will cover all $stg.this, $stg.super etc... AND jvm fields!!!
      case q"${expr: Term}.${name: Term.Name}" =>
        val (evalExpr, envExpr) = evaluate(expr, env)
        (name.defn, evalExpr) match {
          // Some more unary operations
          case (q"..$mods def toChar: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toChar")(e.value), envExpr)
          case (q"..$mods def toByte: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toByte")(e.value), envExpr)
          case (q"..$mods def toShort: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toShort")(e.value), envExpr)
          case (q"..$mods def toInt: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toInt")(e.value), envExpr)
          case (q"..$mods def toLong: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toLong")(e.value), envExpr)
          case (q"..$mods def toFloat: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toFloat")(e.value), envExpr)
          case (q"..$mods def toDouble: $tpeopt = ${expr: Term}", e: Literal) =>
            (invokePrimitiveUnaryMethod("toDouble")(e.value), envExpr)

          // The real work
          case (q"this", e: Instance) => ???
          case (q"..$mods val ..$pats: $tpeopt = ${expr: Term}", e: Instance) => (e.fields(Local(name)), envExpr)
          case (q"..$mods var ..$pats: $tpeopt = $expropt", e: Instance) if expropt.isDefined => (e.fields(Local(name)), envExpr)

          case (q"..$mods def $name: $tpeopt = ${expr: Term}", _) => (evalExpr, envExpr)
          case (q"..$mods def $name(): $tpeopt = ${expr: Term}", _) => (evalExpr, envExpr)
        }
      // Application <expr>(<aexprs>) == <expr>.apply(<aexprs)
        // Same as infix but with method apply
        // If name is a class, then use reflection to create the object
      case q"${name: Term.Name}(..$aexprs)" if name.isClass =>

        ???
      case q"${name0: Term.Name}(..$aexprs)" =>
        val Function(name, args, code) = env(Local(name0))
        ???
      case q"${expr: Term}(..$aexprs)" =>
        evaluate(q"$expr apply (..$aexprs)", env)

      case q"$expr[$_]" => evaluate(expr, env)

      // Infix application to one argument
      case q"${expr0: Term} ${name: Term.Name} ${expr1: Term.Arg}" =>
        // println(name.toString)
        // println(evaluate(expr0, env)._1)
        val (caller: Literal, callerEnv: Environment) = evaluate(expr0, env)
        val (arg: Literal, argEnv: Environment) = evaluate(expr1 match {
          case arg"$name = $expr" => expr
          case arg"$expr: _*" => expr
          case expr: Term => expr
        }, callerEnv)
        val (result: Value, resultEnv: Environment) = name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = ${expr2: Term}" =>
            name.defn.asInstanceOf[m.Member].ffi match {
              case Intrinsic(className: String, methodName: String, signature: String) =>
                (invokePrimitiveBinaryMethod(methodName)(caller.value, arg.value), argEnv)
              case JvmMethod(className: String, fieldName: String, signature: String) =>
                ???
              case Zero => None
            }
        }
        (result, resultEnv.pop._2)

      case q"${expr: Term} ${name: Term.Name} (..$aexprs)" =>
        // println(s"aexprs: $aexprs")
        val (caller, callerEnv) = evaluate(expr, env)
        val paramss: Seq[Term.Param] = name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = $expr" => paramss
        }
        // println(paramss)
        // TODO need to be careful with the different ways to use arguments but let's do it like this for now
        ???

      // Unary application: !<expr> | ~<expr> | +<expr> | -<expr>
      case q"!${expr: Term}" =>
        val (evaluatedExpr: Literal, exprEnv) = evaluate(expr, env)
        (invokePrimitiveUnaryMethod("$bang")(evaluatedExpr.value), exprEnv)
      case q"~${expr: Term}" =>
        val (evaluatedExpr: Literal, exprEnv) = evaluate(expr, env)
        (invokePrimitiveUnaryMethod("$tilde")(evaluatedExpr.value), exprEnv)
      case q"+${expr: Term}" =>
        val (evaluatedExpr: Literal, exprEnv) = evaluate(expr, env)
        (invokePrimitiveUnaryMethod("$plus")(evaluatedExpr.value), exprEnv)
      case q"-${expr: Term}" =>
        val (evaluatedExpr: Literal, exprEnv) = evaluate(expr, env)
        (invokePrimitiveUnaryMethod("$minus")(evaluatedExpr.value), exprEnv)

      case q"${ref: Term.Ref} = ${expr: Term}" =>
        val (evaluatedRef, refEnv) = evaluate(ref, env)
        val (evaluatedExpr, exprEnv) = evaluate(expr, refEnv)
        ???

      case q"{ ..$stats}" =>
        val lastFrame: Frame = env.get
        val blockEnv = env push lastFrame

        val (l: List[Value], newEnv: Environment) = stats.foldLeft((List[Value](), blockEnv)) {
          case ((evaluatedExprs, exprEnv), nextExpr) =>
            // println(nextExpr)
            nextExpr match {
              case q"..$mods def $name: $tpeopt = $expr" =>
                (Literal(()) :: evaluatedExprs, exprEnv + (Local(name), Function(name, Nil, expr)))
              case q"..$mods def $name(..$params): $tpeopt = $expr" =>
                (Literal(()) :: evaluatedExprs, exprEnv + (Local(name), Function(name, params, expr)))
              case q"..$mods val ..$pats: $tpeopt = $expr" =>
                (Literal(()) :: evaluatedExprs, link(pats, expr, exprEnv))
              case q"..$mods var ..$pats: $tpeopt = $expropt" if expropt.isDefined =>
                (Literal(()) :: evaluatedExprs, link(pats, expropt.get, exprEnv))
              case expr: Term =>
                val (res, e) = evaluate(expr, exprEnv)
                (List(res), e)
            }
        }
        // newEnv.propagateChanges
        (l.head, newEnv.pop._2)

      case t => (Literal(null), env)
    }
  }

  private def link(pats: Seq[Pat], expr: Term, env: Environment)(implicit c: Context): Environment = {
    pats.foldLeft(env) {
      case (newEnv, p"_") => evaluate(expr, newEnv)._2

      // TODO Be careful with top level vs not top level patterns
      // TODO Top level are Pat.Var.Term and not top level are Term.Name
      // TODO Think of the val X = 2; val Y = 3; val (X, Y) = (2, 4) example

      case name: Term.Name => ???
      case (newEnv, m: Pat.Var.Term) =>
        val (evaluatedExpr: Value, exprEnv) = evaluate(expr, env)
        exprEnv + (Local(m.name), evaluatedExpr)

      case (newEnv, p"(..$pats0)") => expr match {
        case q"(..$exprs)" => (pats0 zip exprs).foldLeft(newEnv) {
          case (newNewEnv: Environment, (pat, e)) => link(Seq(pat), e, newNewEnv)
        }
      }

      case (newEnv, p"$ref(..$apats)") =>
        println(ref)
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
        // TODO Redo all that
//        (justPats zip justArgExprs).foldLeft(newEnv) {
//          case (newNewEnv: Environment, (seqPat, e: Term)) => link(seqPat, e, newNewEnv)
//        }
        ???
    }
  }

  private def find(toFind: String)(tree: Tree): Unit = tree.topDownBreak.collect {
    case t@q"$expr.$name" if (name: Term.Name).toString == toFind =>
      println(s"Found one $toFind in expression: $t")
      find(toFind)(expr)
    case t: Term.Name if t.toString == toFind =>
      println(s"Found one $toFind in expression: $t")
  }
}
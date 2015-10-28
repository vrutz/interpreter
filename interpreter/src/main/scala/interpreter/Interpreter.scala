package scala.meta
package interpreter

import representations._
import representations.JVMSignature
import representations.JVMSignatureParser._

import scala.util.parsing.combinator._
import scala.util.parsing.input.CharSequenceReader

import scala.collection.mutable.ListBuffer

import scala.reflect.runtime.{universe => ru}

import scala.meta.internal.{ast => m}
import scala.meta.internal.ffi.{Ffi => f}

import java.lang.reflect.Modifier

object Interpreter {

  def eval(stat: Stat, args: Array[String] = Array[String]())(implicit ctx: Context): Value = ctx.typecheck(stat) match {
    // Stat can be either a block (multiple statements like class/object def and imports etc...)
    // Or it is an object containing the main function

    case q"object $name extends $template" =>
      val template"{ ..$_ } with ..$_ { $_ => ..$stats1 }" = template
      val env: Environment = stats1.foldLeft(new Environment()) {
        case (env, q"..$mods def main(${argsName: Term.Name}: Array[String]): Unit = ${ expr:Term }") =>
          env + (MainFun, Main(Instance(args), expr)) + (Local(argsName), Instance(args))
        case (env, q"..$mods def $name[..$tparams](..$params): $tpeopt = $expr") => 
          env + (Local(name), Function(name, params, expr))
      }

      val Main(arguments, term) = env(MainFun)

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
      val res = term match {
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
         case i: Instance=> (i, env)
        }

    // Selection <expr>.<name>
    // Will cover all $stg.this, $stg.super etc... AND jvm fields!!!
      case q"${expr: Term}.${name: Term.Name}" =>
        val (evalExpr, envExpr) = evaluate(expr, env)
        (name.defn, evalExpr) match {
          // All intrinsic operations such as toChar, toInt, length, ...
          case (_, Instance(jvmInstance)) if getFFI(name).isInstanceOf[f.Intrinsic] =>
            (invokePrimitiveUnaryMethod(name.toString)(jvmInstance), envExpr)

          // The real work
          case (q"this", e: Instance) => ???
          // Use reflection to get fields etc...
          // case (q"..$mods val ..$pats: $tpeopt = ${expr: Term}", e: Instance) => (e.fields(Local(name)), envExpr)
          // case (q"..$mods var ..$pats: $tpeopt = $expropt", e: Instance) if expropt.isDefined => (e.fields(Local(name)), envExpr)

          case (q"..$mods def $name: $tpeopt = ${expr: Term}", _) => (evalExpr, envExpr)
          case (q"..$mods def $name(): $tpeopt = ${expr: Term}", _) => (evalExpr, envExpr)
        }
      // Application <expr>(<aexprs>) == <expr>.apply(<aexprs)
        // Same as infix but with method apply
        // If name is a class, then use reflection to create the object
      case q"${name: Term.Name}[$_]()" if name.isClass =>
        val c: Class[_] = Class.forName(name.toString)
        (Instance(c.newInstance), env)
      case q"${name: Term.Name}()" if name.isClass =>
        val c: Class[_] = Class.forName(name.toString)
        (Instance(c.newInstance), env)

      case q"${name: Term.Name}(..$aexprs)" if name.isClass =>
        val c: Class[_] = Class.forName(name.toString)
        // val types: Array[Class[_]] = aexprs.map(aexpr => Class.forName(aexpr.internalTyping.get.))
        // val ctor = c.getDeclaredConstructor(types)
        ???
      case q"${name0: Term.Name}(..$aexprs)" if getFFI(name0) != f.Zero =>
        val justExprsBuffer: ListBuffer[Value] = ListBuffer[Value]()
        val argEnv: Environment = aexprs.foldLeft(env) {
          case (e, arg"$name = $expr") => 
            val (newExpr, newEnv) = evaluate(expr, e)
            justExprsBuffer += newExpr
            newEnv
          case (e, arg"$expr: _*") => 
            val (newExpr, newEnv) = evaluate(expr, e)
            justExprsBuffer += newExpr
            newEnv
          case (e, expr: Term) => 
            val (newExpr, newEnv) = evaluate(expr, e)
            justExprsBuffer += newExpr
            newEnv
        }
        val justExprs = justExprsBuffer.toArray

        getFFI(name0) match {
          case f.Intrinsic(className: String, methodName: String, signature: String) =>
            ???

          case f.JvmMethod(className: String, fieldName: String, signature: String) =>
            val c: Class[_] = Class.forName(jvmToFullName(className))
            val argsType: Array[Class[_ <: Any]] = parsing(signature).getSignature
            val method = c.getMethod(fieldName, argsType: _*)
            val module = c.getField("MODULE$").get(c)

            (method.invoke(module, justExprs: _*) match {
              case null => Literal(())
              case _ => ???
            }, argEnv)
        }

      case q"${expr: Term}(..$aexprs)" =>
        evaluate(q"$expr apply (..$aexprs)", env)

      case q"$expr[$_]" => evaluate(expr, env)

      // Infix application to one argument
      case q"${expr0: Term} ${name: Term.Name} ${expr1: Term.Arg}" =>
        val (caller: Literal, callerEnv: Environment) = evaluate(expr0, env)
        val (arg: Literal, argEnv: Environment) = evaluate(expr1 match {
          case arg"$name = $expr" => expr
          case arg"$expr: _*" => expr
          case expr: Term => expr
        }, callerEnv)
        val (result: Value, resultEnv: Environment) = name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = ${expr2: Term}" =>
            name.defn.asInstanceOf[m.Member].ffi match {
              case f.Intrinsic(className: String, methodName: String, signature: String) =>
                (invokePrimitiveBinaryMethod(methodName)(caller.value, arg.value), argEnv)
              case f.JvmMethod(className: String, fieldName: String, signature: String) =>
                ???
              case f.Zero => None
            }
        }
        (result, resultEnv.pop._2)

      case q"${expr: Term} ${name: Term.Name} (..$aexprs)" =>
        val (caller, callerEnv) = evaluate(expr, env)
        val paramss: Seq[Term.Param] = name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = $expr" => paramss
        }
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
    // println(s"$term evaluates to ${res._1}")
    res
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

  private def jvmToFullName(jvmName: String): String = jvmName.substring(1, jvmName.length - 1).replace('/', '.')

  private def getFFI(name: Term.Name)(implicit ctx: Context) = name.defn.asInstanceOf[m.Member].ffi

  implicit val parser = signature

  private def parsing[T](s: String)(implicit p: Parser[T]):T = {
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
}
package scala.meta
package internal
package interpreter

import scala.meta.internal.representations._
import scala.meta.internal.representations.JVMSig
import scala.meta.internal.representations.JVMSig._


import scala.collection.mutable.ListBuffer

import scala.reflect.runtime.{universe => ru}

import scala.meta.internal.{ast => m}
import scala.meta.internal.ffi.{Ffi => f}

import java.lang.reflect.Modifier

object Interpreter {

  private[meta] def evaluate(term: Term, env: Environment = new Environment())(implicit ctx: Context): (Value, Environment) = {
    // println(s"to evaluate: $term")
    // println(s"Env: $env")
    val res = term match {
      /* Literals */
      case x: Lit => (Literal(x.value), env)

      /* Expressions */
      // if (<expr>) expr else <expr>
      case q"if ($cond) $thn else $els" =>
      evaluate(cond, env) match {
        case (Literal(true), e) => evaluate(thn, e)
        case (Literal(false), e) => evaluate(els, e)
        case (_, e) => (null, e) // Evaluation exception
      }

      // this
      case q"this" => (env(This), env)

      //  super: super | super[<expr>]
      case q"super" => (env(Super), env)
      case q"super[$_]" => (env(Super), env)

      case name: Term.Name => env(Local(name)) match {
         case l @ Literal(_) => (l, env)
         case f @ Function(name, Nil, expr) => evaluate(expr, env)
         case f @ Function(name, args, expr) => (f, env)
        }

    // Selection <expr>.<name>
    // Will cover all $stg.this, $stg.super etc... AND jvm fields!!!
      case q"${expr: Term}.${name: Term.Name}" =>
        val (evalExpr, envExpr) = evaluate(expr, env)
        (name.defn, evalExpr) match {
          // All intrinsic operations on arrays such as length, apply ...
          case (_, Literal(jvmInstance)) if getFFI(name).isInstanceOf[f.Intrinsic] && jvmInstance.getClass.isArray => 
            // println(Literal(jvmInstance))
            (invokeArrayMethod(name.toString)(jvmInstance.asInstanceOf[AnyRef]), envExpr)

          // All intrinsic operations such as toChar, toInt, ...
          case (_, Literal(lit)) if getFFI(name).isInstanceOf[f.Intrinsic] =>
            val f.Intrinsic(className, methodName, signature) = getFFI(name)
            if (className.head != 'L') {
              (invokePrimitiveUnaryMethod(methodName)(lit), envExpr)
            } else {
              (invokeObjectUnaryMethod(methodName)(lit) , envExpr)
            }

          case (q"this", e: Literal) => (evalExpr, envExpr)
          // Use reflection to get fields etc...
          // case (q"..$mods val ..$pats: $tpeopt = ${expr: Term}", e: Instance) =>
          // case (q"..$mods var ..$pats: $tpeopt = $expropt", e: Instance) if expropt.isDefined => (e.fields(Local(name)), envExpr)

          case (q"..$mods def $name: $tpeopt = ${expr: Term}", _) => 
            val e = envExpr push envExpr.get
            val (res, resEnv) = evaluate(q"$expr", e + (This, evalExpr))
            (res, resEnv.pop._2)
          case (q"..$mods def $name(): $tpeopt = ${expr: Term}", _) => 
            val e = envExpr push envExpr.get
            val (res, resEnv) = evaluate(q"$expr", e + (This, evalExpr))
            (res, resEnv.pop._2)
        }
      // Application <expr>(<aexprs>) == <expr>.apply(<aexprs)
        // Same as infix but with method apply
        // If name is a class, then use reflection to create the object
      case q"${name: Ctor.Name}[$_]()" =>
        val c: Class[_] = Class.forName(name.toString)
        (Literal(c.newInstance), env)
      case q"${name: Ctor.Name}()" =>
        val c: Class[_] = Class.forName(name.toString)
        (Literal(c.newInstance), env)

      case q"${name: Ctor.Name}(..$aexprs)" =>
        val c: Class[_] = Class.forName(name.toString)
        val (args, argsEnv) = evaluateArguments(aexprs, env)
        val argsTypes = args.map { case Literal(l) => l.getClass }.toArray
        val ctor = c.getDeclaredConstructor(argsTypes: _*)
        (ctor.newInstance(args.map { case Literal(l) => l }), argsEnv)

      case q"${name: Term.Name}(..$aexprs)" =>
        getFFI(name) match {
          // Compiled function
          case f.Intrinsic(className: String, methodName: String, signature: String) =>
            // Evaluate caller
            // println(s"Caller $name\nClassName: $className\nMethod: $methodName")
            // println(s"Env is $env")
            // println(s"Args in env: ${env(Local(name))}")
            val (Literal(callerJVM), callerEnv) = evaluate(name, env)

            // Evaluate arguments
            val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, callerEnv)

            // Call the right method given the type of the caller
            if (callerJVM.getClass.isArray)
              (invokeArrayMethod(methodName)(callerJVM.asInstanceOf[AnyRef], args map {
                case Literal(l) => l
              }: _*), callerEnv)
            else
              (invokeObjectBinaryMethod(methodName)(callerJVM, args(0) match {
                case Literal(l) => l
              }), callerEnv)

          // Compiled function
          case f.JvmMethod(className: String, fieldName: String, signature: String) =>
            // Evaluate arguments
            val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, env)

            // Get class, and the right method
            val c: Class[_] = Class.forName(jvmToFullName(className))
            val argsType: List[Class[_ <: Any]] = parsing(signature).arguments
            val method = c.getMethod(fieldName, argsType: _*)
            val module = c.getField("MODULE$").get(c)

            // Call the method
            (method.invoke(module, args: _*) match {
              case null => Literal(())
              case res => Literal(res)
            }, argsEnv)

          // User defined function
          case f.Zero =>
            // get the function from the environment
            evaluateFunction(env(Local(name)).asInstanceOf[Function], aexprs, env)
        }

      case q"${expr: Term}[$_]" => evaluate(expr, env)

      // Infix application to one argument
      case q"${expr0: Term} ${name: Term.Name} ${expr1: Term.Arg}" =>
        val (caller: Literal, callerEnv: Environment) = evaluate(expr0, env)
        val (arg: Literal, argEnv: Environment) = evaluate(extractExprFromArg(expr1), callerEnv)

        name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = ${expr2: Term}" =>
            getFFI(name) match {
              case f.Intrinsic(className: String, methodName: String, signature: String) =>
                (invokePrimitiveBinaryMethod(methodName)(caller.value, arg.value), argEnv)
              case f.JvmMethod(className: String, fieldName: String, signature: String) =>
                ???
              case f.Zero =>
                ???
            }
        }

      case q"${expr: Term} ${name: Term.Name} (..${aexprs: Seq[Term.Arg]})" =>
        // Evaluate the caller
        val (caller, callerEnv) = evaluate(expr, env)
        // Evaluate the arguments
        val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, callerEnv)

        // Find out what kind of function $name is
        name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = $expr" =>
            getFFI(name) match {
              case f.Intrinsic(className: String, methodName: String, signature: String) =>
                // If intrinsic, either an instance or an array
                caller match {
                  case Literal(array) if array.getClass.isArray =>
                    (invokeArrayMethod(name.toString)(array.asInstanceOf[AnyRef], args.map {
                      case Literal(l) => l
                    }: _*), argsEnv)
                  case Literal(o) => (invokeObjectBinaryMethod(name.toString)(o, args(0) match {
                      case Literal(l) => l
                    }), argsEnv)
                }
              case f.JvmMethod(className: String, fieldName: String, signature: String) =>
                ???
              case f.Zero =>
                ???
            }
        }
      case q"${expr: Term}(..$aexprs)" =>
        val (fun @ Function(name, params, code), evalEnv) = evaluate(expr, env)
        evaluateFunction(fun, aexprs, evalEnv)

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

      case (newEnv, name: Term.Name) =>
        val (evaluatedExpr: Value, exprEnv) = evaluate(expr, env)
          exprEnv + (Local(name), evaluatedExpr)
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
            aexprs map extractExprFromArg
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

  private def jvmToFullName(jvmName: String): String = jvmName.substring(1, jvmName.length - 1).replace('/', '.')

  private def getFFI(name: Term.Name)(implicit ctx: Context) = name.defn.asInstanceOf[m.Member].ffi

  private def evaluateArguments(args: Seq[Term.Arg], env: Environment)
    (implicit ctx: Context): (Array[Value], Environment) = {
    val argsBuffer: ListBuffer[Value] = ListBuffer[Value]()
      val argEnv: Environment = args.map(extractExprFromArg).foldLeft(env) {
        case (e, expr: Term) => 
          val (newExpr: Value, newEnv) = evaluate(expr, e)
          argsBuffer += newExpr
          newEnv
      }
      (argsBuffer.toArray, argEnv)
  }

  private def evaluateFunction(fun: Function, aexprs: Seq[Term.Arg], env: Environment)
    (implicit ctx: Context) = {
    // Evaluate the arguments and add them to the environment
     val Function(funName, args: Seq[Term.Param], code) = fun
    val (argsValues: Array[Value], evArgsEnv: Environment) = evaluateArguments(aexprs, env)

    val argsEnv = (args zip argsValues.toSeq).foldLeft(evArgsEnv) {
      case (e, (param"..$mods $paramname: $atpeopt = $expropt", av)) => 
        paramname match {
          case nameParam: Term.Name => e + (Local(nameParam), av)
        }
    }
    evaluate(code, argsEnv)
  }

  private def extractExprFromArg(expr0: Term.Arg) = {
    expr0 match {
      case arg"$name = $expr" => expr
      case arg"$expr: _*" => expr
      case expr: Term => expr
    }
  }
}
package scala.meta
package internal
package interpreter

import scala.meta.internal.representations._
import scala.meta.internal.representations.JVMSig
import scala.meta.internal.representations.JVMSig._
import scala.runtime.ScalaRunTime._


import scala.collection.mutable.ListBuffer

import scala.reflect.runtime.{universe => ru}

import scala.meta.internal.{ast => m}
import scala.meta.internal.ffi.{Ffi => f}

import java.lang.reflect.Modifier

object Interpreter {

  var debug: Boolean = false

  private def evaluateLiteral(term: Lit, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    (Val(term.value), env)
  }

  private def evaluateIf(term: m.Term.If, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val q"if ($cond) ${thn: Term} else ${els: Term}" = term
    val (Val(condVal: Boolean), e) = evaluate(cond, env)
    if(condVal) {
      evaluate(thn, e)
    } else {
      evaluate(els, e)
    }
  }

  private def evaluateBlock(term: m.Term.Block, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    term match {
      case q"{ ..$stats}" =>
        val lastFrame: Frame = env.get
        val blockEnv = env push lastFrame

        val (l: List[Value], newEnv: Environment) = stats.foldLeft((List[Value](), blockEnv)) {
          case ((evaluatedExprs, exprEnv), nextExpr) =>
            nextExpr match {
              case q"..$mods def $name: $tpeopt = $expr" =>
                (Val(()) :: evaluatedExprs, exprEnv + (Local(name), Function(Some(name), Nil, expr)))
              case q"..$mods def $name(..$params): $tpeopt = $expr" =>
                (Val(()) :: evaluatedExprs, exprEnv + (Local(name), Function(Some(name), params, expr)))
              case q"..$mods val ..$pats: $tpeopt = $expr" =>
                (Val(()) :: evaluatedExprs, link(pats, expr, exprEnv))
              case q"..$mods var ..$pats: $tpeopt = $expropt" if expropt.isDefined =>
                (Val(()) :: evaluatedExprs, link(pats, expropt.get, exprEnv))
              case expr: Term =>
                val (res, e) = evaluate(expr, exprEnv)
                (res :: evaluatedExprs, e)
            }
        }
        (l.head, newEnv.pop._2)
    }
  }

  private def evaluateSelection(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val q"${expr: Term}.${name: Term.Name}" = term

    eprintln(getFFI(name))
    getFFI(name) match {
      case f.Intrinsic(className: String, methodName: String, signature: String) =>
        val (Val(evalVal), envExpr) = evaluate(expr, env)
        if(evalVal.getClass.isArray) {
          (invokeArrayMethod(name.toString)(evalVal.asInstanceOf[AnyRef]), envExpr)
        } else if(className.head != 'L') {
          (invokePrimitiveUnaryMethod(methodName)(evalVal), envExpr)
        } else {
          (invokeObjectUnaryMethod(methodName)(evalVal) , envExpr)
        }

      case f.JvmMethod(className: String, fieldName: String, signature: String) =>
        eprintln(className)
        eprintln(fieldName)
        eprintln(signature)
        val (Val(evalVal), envExpr) = evaluate(expr, env)
        val c = Class.forName(jvmToFullName(className))
        name.defn match {
          case q"..$mods def ${nameField: Term.Name}(...$_): $_ = ???" =>
            val m = c.getMethod(fieldName)
            (Val(m.invoke(evalVal)), envExpr)

          case q"..$mods val ${nameField: Term.Name}: $_ = ???" =>
            val f = c.getDeclaredField(fieldName)
            (Val(f.get(evalVal)), envExpr)

          case q"..$mods var ${nameField: Term.Name}: $_ = ???" =>
            val f = c.getDeclaredField(fieldName)
            (Val(f.get(evalVal)), envExpr)
        }

      // TODO: support nested objects
      // this is a top-level object, ignore expr because it's a package reference
      case f.JvmErasure(className: String) if className.endsWith("$;") =>
        val c: Class[_] = Class.forName(jvmToFullName(className))
        (Val(c.getField("MODULE$").get(c)), env)

      // case f.Zero => ??? // Should not happen since no user defined classes
    }
  }

  private def evaluateConstructor(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    term match {
      case q"${name: Ctor.Name}[$_]()" =>
        val c: Class[_] = Class.forName(name.toString)
        (Val(c.newInstance), env)
      case q"${name: Ctor.Name}()" =>
        val c: Class[_] = Class.forName(name.toString)
        (Val(c.newInstance), env)

      case q"${name: Ctor.Name}(..$aexprs)" =>
        val c: Class[_] = Class.forName(name.toString)
        val (args, argsEnv) = evaluateArguments(aexprs, env)
        val argsTypes = args.map { case Val(l) => l.getClass }.toArray
        val ctor = c.getDeclaredConstructor(argsTypes: _*)
        (Val(ctor.newInstance(args.map { case Val(l) => l })), argsEnv)
    }
  }

  private def evaluateApplication(fun0: Term, aexprss0: Seq[Seq[Term.Arg]], env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val fun = {
      def loop(fun0: Term): Term = fun0.desugar match {
        case q"${fun: Term}[..$_]" => loop(fun) // desugarings may contain inferred type arguments
        case fun => fun
      }
      loop(fun0)
    }
    val aexprs = aexprss0.flatten // TODO: support multiple argument lists

    fun match {
      case name: m.Term.Name =>
        // static local method call
        val f = env(Local(name)).asInstanceOf[Function]
        evaluateFunction(f, aexprs, env)
      case m.Term.Select(qual, name) =>
        // other calls are going to be prefixed in one way or another
        val (caller, callerEnv: Environment) = evaluate(qual, env)
        caller match {
          case f: Function =>
            // dynamic local method call
            evaluateFunction(f, aexprs, callerEnv)
          case Val(jvmCaller: AnyRef) =>
            // compiled method call
            val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, callerEnv)
            val result = getFFI(name) match {
              case f.Intrinsic(className: String, methodName: String, signature: String) =>
                val argsType: List[Class[_ <: Any]] = parsing(signature).arguments
                val jvmArgs = extractArgs(args).asInstanceOf[Array[AnyRef]]
                if (jvmCaller.getClass.isArray) {
                  invokeArrayMethod(methodName)(jvmCaller, jvmArgs: _*)
                } else if(className.head != 'L' || className == "Ljava/lang/String;") {
                  if (argsType.length == 0) invokePrimitiveUnaryMethod(methodName)(jvmCaller)
                  else invokePrimitiveBinaryMethod(methodName)(jvmCaller, jvmArgs(0))
                } else {
                  if (argsType.length == 0) invokeObjectUnaryMethod(methodName)(jvmCaller)
                  else invokeObjectBinaryMethod(methodName)(jvmCaller, jvmArgs(0))
                }
              case f.JvmMethod(className: String, fieldName: String, signature: String) =>
                val c: Class[_] = Class.forName(jvmToFullName(className))
                val argsType: List[Class[_ <: Any]] = parsing(signature).arguments
                val method = c.getMethod(fieldName, argsType: _*)
                val jvmArgs = checkArgs(args, argsType).asInstanceOf[Array[AnyRef]]
                Val(method.invoke(jvmCaller, jvmArgs: _*))
            }
            (result, argsEnv)
        }
    }
  }

  private def evaluateLambda(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val q"(..${args: Seq[Term.Param]}) => ${expr: Term}" = term
    (Function(None, args, expr), env)
  }

  private def evaluatePattern(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val (scrutinee, cases: Seq[Case]) = term match {
      case q"${expr: Term} match { ..case $casesnel }" => (expr, casesnel)
    }

    val (scrutineeEval, scrutineeEnv) = evaluate(scrutinee, env)

    eprintln(s"scrutinee: $scrutineeEval")

    val Some(result) = cases.foldLeft(None: Option[(Value, Environment)]) {
      case (None, p"case $pat0 if $expropt => $expr") =>
        def checkPat(pattern: meta.Pat, patEnv: Environment): Option[(Value, Environment)] = pattern match {
            case p"_" => Some(evaluate(expr, patEnv))
            case q"${name: Pat.Var.Term}" => Some(evaluate(expr, patEnv + (Local(name.name), scrutineeEval)))
            case p"$pname @ $apat" => apat match {
                // case parg"_*" =>
                case parg"${pat1: meta.Pat}" => checkPat(pat1, patEnv + (Local(pname.name), scrutineeEval))
              }
            case p"$pat1 | $pat2" =>
              checkPat(pat1, patEnv) match {
                case None => checkPat(pat2, patEnv)
                case r => r
              }
            case p"$pat1: ${ptpe: Pat.Type}" if scrutinee.tpe <:< ptpe.tpe =>
              checkPat(pat1, patEnv)
            case p"${name: Term.Name}" if patEnv(Local(name)) == scrutineeEval => Some(evaluate(expr, patEnv))
            case p"$expr.${name: Term.Name}" =>
              val (patternEval, patternEnv) = evaluate(q"$expr.$name", patEnv)
              if(patternEval == scrutineeEval)
                Some(evaluate(expr, patEnv))
              else None
            case p"${lit: Lit}" if Val(lit.value) == scrutineeEval => Some(evaluate(expr, patEnv))
            case p => eprintln(s"pattern not supported: $p"); None
          }

          val res = checkPat(pat0, scrutineeEnv)

          if (res != None && (expropt == None || evaluate(expropt.get, res.get._2)._1 == Val(true)))
            res
          else None
      case (Some((res, resEnv)), _) => Some((res, resEnv))
    }

    result
  }

  private[meta] def evaluate(term0: Term, env: Environment = new Environment())
    (implicit ctx: Context): (Value, Environment) = {
    val term = term0.desugar
    // eprintln(s"to evaluate: $term")
    // eprintln(s"Env: $env")
    val res = term match {
      // Literal
      case x: Lit => eprintln("Evaluating literal"); evaluateLiteral(x, env)

      // Ifs
      case t: m.Term.If => eprintln("Evaluating if"); evaluateIf(t, env)

      // Name
      case name: Term.Name =>
        eprintln("Evaluating name")
        eprintln(s"Looking for $name in $env")
        eprintln(s"""$name is ${if(!env.contains(Local(name))) "not" else ""} in the environment""")
        env(Local(name)) match {
          case v: Val => (v, env)
          case Function(name, Nil, code) => evaluate(code, env)
          case f: Function => (f, env)
        }

      // Contructors
      case q"${name: Ctor.Name}[..$_](..$aexprs)" => eprintln("Evaluating constructor"); evaluateConstructor(term, env)

      // Application
      case app @ q"${expr: Term}(..${args: Seq[Term.Arg]})" =>
        eprintln("Evaluating apply")
        val argss = {
          // TODO: find out why the ...$argss quasiquote doesn't work
          def loop(expr: Term): Seq[Seq[Term.Arg]] = expr match {
            case q"${expr: Term}(..${args: Seq[Term.Arg]})" => loop(expr) :+ args
            case _ => Nil
          }
          loop(app)
        }
        evaluateApplication(expr, argss, env)

      // Infix application
      case q"${expr: Term} ${name: Term.Name} ${arg0: Term.Arg}" =>
        eprintln("Evaluating apply infix")
        val emulation = q"$expr.$name".asInstanceOf[m.Term].withAttrs(name.tpe).setTypechecked
        val argss = Seq(Seq(arg0))
        // TODO: this doesn't work correctly with right-associative operators
        evaluateApplication(emulation, argss, env)

      // Selection
      case t: m.Term.Select => eprintln("Evaluating selection"); evaluateSelection(term, env)

      // Block
      case t: m.Term.Block => eprintln("Evaluating block"); evaluateBlock(t, env)

      // Lambda
      case q"(..${args: Seq[Term.Param]}) => $expr" => eprintln("Evaluating lambda"); evaluateLambda(term, env)

      // Patterns
      case q"${expr: Term} match { ..case $casesnel }" => eprintln("Evaluating pattern matching"); evaluatePattern(term, env)

      case q"${expr: Term}[$_]" => evaluate(expr, env)

      // Safety Net
      case _ => (Val(null), env)
    }
    // eprintln(s"$term evaluates to ${res._1}")
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

  def evaluateFunction(f: Function, aexprs: Seq[Term.Arg], env: Environment)
    (implicit ctx: Context): (Value, Environment) = {
    val Function(_, params: Seq[Term.Param], code) = f
    val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, env)
    val callEnv = (params zip args.toSeq).foldLeft(argsEnv) {
      case (e, (param"..$mods ${nameParam: Term.Name}: $atpeopt = $expropt", av)) =>
        e + (Local(nameParam), av)
    }
    evaluate(code, callEnv)
  }

  private def extractArgs(args: Array[Value]) = args map {
      case Val(v) => v
    }

  private def checkArgs(args: Array[Value], argsType: List[Class[_ <: Any]]): Array[Any] = {
    require(args.length >= argsType.size)
    if (args.length == argsType.size) {
      if (box(argsType.last).isAssignableFrom(extractArgs(args)(args.length - 1).getClass)) {
        extractArgs(args)
      } else {
        args.update(args.length - 1, args(args.length - 1) match {
            case Val(v) => Val(Seq(v))
          })
        extractArgs(args)
      }
    } else {
      val newArgs = new Array[Value](argsType.size)
      for (i <- 0 until argsType.size - 1) {
        newArgs.update(i, args(i))
      }
      val repArgs = ListBuffer[Any]()
      for (i <- argsType.size - 1 until args.length) {
        repArgs += (args(i) match {
          case Val(v) => v
        })
      }
      newArgs.update(argsType.size - 1, Val(repArgs.toSeq))
      extractArgs(newArgs)
    }
  }

  private def extractExprFromArg(expr0: Term.Arg) = {
    expr0 match {
      case arg"$name = $expr" => expr
      case arg"$expr: _*" => expr
      case expr: Term => expr
    }
  }

  private def eprintln(s: Any) = {
    if(debug) println(s)
  }
}
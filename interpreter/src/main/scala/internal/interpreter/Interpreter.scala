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

  private def evaluateLiteral(term: Lit, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    (Val(term.value), env)
  }

  private def evaluateIf(term: m.Term.If, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    term match {
      case q"if ($cond) ${thn: Term} else ${els: Term}" =>
        val (Val(condVal: Boolean), e) = evaluate(cond, env)
        if(condVal) {
          evaluate(thn, e)
        } else {
          evaluate(els, e)
        }
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
        // newEnv.propagateChanges
        (l.head, newEnv.pop._2)
    }
  }

  private def evaluateSelection(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    term match {
      case q"${expr: Term}.${name: Term.Name}" =>
        val (evalExpr, envExpr) = evaluate(expr, env)
        (name.defn, evalExpr) match {
          // All intrinsic operations on arrays such as length, apply ...
          case (_, Val(jvmInstance)) if getFFI(name).isInstanceOf[f.Intrinsic] && jvmInstance.getClass.isArray => 
            // println(Val(jvmInstance))
            (invokeArrayMethod(name.toString)(jvmInstance.asInstanceOf[AnyRef]), envExpr)

          // All intrinsic operations such as toChar, toInt, ...
          case (_, Val(lit)) if getFFI(name).isInstanceOf[f.Intrinsic] =>
            val f.Intrinsic(className, methodName, signature) = getFFI(name)
            if (className.head != 'L') {
              (invokePrimitiveUnaryMethod(methodName)(lit), envExpr)
            } else {
              (invokeObjectUnaryMethod(methodName)(lit) , envExpr)
            }

          case (q"this", e: Val) => (e, envExpr)

          // Use reflection to get fields etc...
          case (q"..$mods val ..$pats: $tpeopt = ${expr: Term}", Val(instance)) =>
            val c = instance.getClass
            val field = c.getDeclaredField(pats.toString)
            val value = Val(field.get(instance).asInstanceOf[Any])
            (value, envExpr)
          case (q"..$mods var ..$pats: $tpeopt = $expropt", Val(instance)) =>
            val c = instance.getClass
            val field = c.getDeclaredField(pats.toString)
            val value = Val(field.get(instance).asInstanceOf[Any])
            (value, envExpr)

          case (q"..$mods def $name: $tpeopt = ${expr: Term}", _) => 
            val e = envExpr push envExpr.get
            val (res, resEnv) = evaluate(q"$expr", e + (This, evalExpr))
            (res, resEnv.pop._2)
          case (q"..$mods def $name(): $tpeopt = ${expr: Term}", _) => 
            val e = envExpr push envExpr.get
            val (res, resEnv) = evaluate(q"$expr", e + (This, evalExpr))
            (res, resEnv.pop._2)
        }
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

  private def evaluateApplication(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    require(term.isInstanceOf[m.Term.Apply] || term.isInstanceOf[m.Term.ApplyInfix] || term.isInstanceOf[m.Term.ApplyUnary])
    term match {
      case q"${name: Term.Name}(..$aexprs)" =>
        println(getFFI(name))
        getFFI(name) match {
          // Compiled function
          case f.Intrinsic(className: String, methodName: String, signature: String) =>
            // Evaluate caller
            // println(s"Caller $name\nClassName: $className\nMethod: $methodName")
            // println(s"Env is $env")
            // println(s"Args in env: ${env(Local(name))}")
            val (Val(callerJVM), callerEnv) = evaluate(name, env)

            // Evaluate arguments
            val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, callerEnv)

            // Call the right method given the type of the caller
            if (callerJVM.getClass.isArray) {
              (invokeArrayMethod(methodName)(callerJVM.asInstanceOf[AnyRef], args map {
                case Val(l) => l
              }: _*), callerEnv)
            } else {
              (invokeObjectBinaryMethod(methodName)(callerJVM, args(0) match {
                case Val(l) => l
              }), callerEnv)
            }

          // Compiled function
          case f.JvmMethod(className: String, fieldName: String, signature: String) =>
            // Evaluate arguments
            val (args: Array[Value], argsEnv: Environment) = evaluateArguments(aexprs, env)

            // Get class, and the right method
            val c: Class[_] = Class.forName(jvmToFullName(className))
            val argsType: List[Class[_ <: Any]] = parsing(signature).arguments

            val typeCompliantArgs = checkArgs(args, argsType).asInstanceOf[Array[Object]]

            val method = c.getMethod(fieldName, argsType: _*)
            val module = c.getField("MODULE$").get(c)

            // Call the method
            (Val(method.invoke(module, typeCompliantArgs: _*)), argsEnv)

          // User defined function
          case f.Zero =>
            // get the function from the environment
            evaluateFunction(env(Local(name)).asInstanceOf[Function], aexprs, env)
        }

      case q"${expr0: Term} ${name: Term.Name} ${arg0: Term.Arg}" =>
        val (caller: Val, callerEnv: Environment) = evaluate(expr0, env)
        val (arg: Val, argEnv: Environment) = evaluate(extractExprFromArg(arg0), callerEnv)
        name.defn match {
          case q"..$mods def $name[..$tparams](..$paramss): $tpeopt = ${expr2: Term}" =>
            getFFI(name) match {
              case f.Intrinsic(className: String, methodName: String, signature: String) =>
                if (caller.value.getClass.isArray) {
                  (invokeArrayMethod(name.toString)(caller.value.asInstanceOf[AnyRef], arg.value), callerEnv)
                } else if(className.head != 'L' || className == "Ljava/lang/String;"){
                  (invokePrimitiveBinaryMethod(methodName)(caller.value, arg.value), argEnv)
                } else {
                  (invokeObjectBinaryMethod(methodName)(caller.value, arg.value), argEnv)
                }
              case f.JvmMethod(className: String, fieldName: String, signature: String) =>
                // Get class, and the right method
                val c: Class[_] = Class.forName(jvmToFullName(className))
                val argsType: List[Class[_ <: Any]] = parsing(signature).arguments

                val method = c.getMethod(fieldName, argsType: _*)

                (Val(method.invoke(caller.value, arg.value.asInstanceOf[Object])), argEnv)

              case f.Zero =>
                evaluateFunction(env(Local(name)).asInstanceOf[Function], Seq(arg0), env)

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
                  case Val(array) if array.getClass.isArray =>
                    (invokeArrayMethod(name.toString)(array.asInstanceOf[AnyRef], args.map {
                      case Val(l) => l
                    }: _*), argsEnv)
                  case Val(o) => 
                    if(className.head == 'L') {
                      (invokeObjectBinaryMethod(name.toString)(o, args(0) match {
                        case Val(l) => l
                      }), argsEnv)
                    } else {
                      (invokePrimitiveBinaryMethod(name.toString)(o, args(0) match {
                        case Val(v) => v
                      }), argsEnv)
                    }
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
  }
}

  private def evaluateLambda(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val q"(..${args: Seq[Term.Param]}) => ${expr: Term}" = term
    println(args)
    println(expr)
    (Function(None, args, expr), env)
  }
  private def evaluatePattern(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = {
    val (scrutinee, cases: Seq[Case]) = term match {
      case q"${expr: Term} match { ..case $casesnel }" => (expr, casesnel)
    }

    val (scrutineeEval, scrutineeEnv) = evaluate(scrutinee, env)

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
            // case p"(..$patsnel)" => ???
            // case p"$pat1 $name (..$apatsnel)" => ???
            case p"$pat1: $ptpe" =>
              checkPat(pat1, patEnv)
            case p"${name: Term.Name}" if patEnv(Local(name)) == scrutineeEval => Some(evaluate(expr, patEnv))
            case p"$expr.${name: Term.Name}" =>
              val (patternEval, patternEnv) = evaluate(q"$expr.$name", patEnv)
              if(patternEval == scrutineeEval)
                Some(evaluate(expr, patEnv))
              else None
            case p"${lit: Lit}" if Val(lit.value) == scrutineeEval => Some(evaluate(expr, patEnv))
            case _ => None
          }

          val res = checkPat(pat0, scrutineeEnv)

          if (res != None && (expropt == None || evaluate(expropt.get, res.get._2)._1 == Val(true)))
            res
          else None
      case (Some((res, resEnv)), _) => Some((res, resEnv))
    }

    result
  }

  private[meta] def evaluate(term: Term, env: Environment = new Environment())(implicit ctx: Context): (Value, Environment) = {
    // println(s"to evaluate: $term")
    // println(s"Env: $env")
    val res = term match {
      // Literal
      case x: Lit => println("Evaluating literal"); evaluateLiteral(x, env)

      // Ifs
      case t: m.Term.If => println("Evaluating if"); evaluateIf(t, env)

      // Name
      case name: Term.Name => println("Evaluating name"); env(Local(name)) match {
         case l @ Val(_) => (l, env)
         case f @ Function(name, Nil, expr) => evaluate(expr, env)
         case f @ Function(name, args, expr) => (f, env)
        }

      // Contructors
      case q"${name: Ctor.Name}[..$_](..$aexprs)" => println("Evaluating constructor"); evaluateConstructor(term, env)

      // Application
      case t: m.Term.Apply => println("Evaluating apply"); evaluateApplication(t, env)
      case t: m.Term.ApplyInfix => println("Evaluating apply infix"); evaluateApplication(t, env)
      case t: m.Term.ApplyUnary => println("Evaluating apply unary"); evaluateApplication(t, env)

      // Selection
      case t: m.Term.Select => println("Evaluating selection"); evaluateSelection(term, env)

      // Block
      case t: m.Term.Block => println("Evaluating block"); evaluateBlock(t, env)

      // Lambda
      case q"(..${args: Seq[Term.Param]}) => $expr" => println("Evaluating lambda"); evaluateLambda(term, env)

      // Patterns
      case q"${expr: Term} match { ..case $casesnel }" => println("Evaluating pattern matching"); evaluatePattern(term, env)

      case q"${expr: Term}[$_]" => evaluate(expr, env)

      // Safety Net
      case _ => (Val(null), env)
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

  private def extractArgs(args: Array[Value]) = args map {
      case Val(v) => v
    }

  private def checkArgs(args: Array[Value], argsType: List[Class[_ <: Any]]): Array[Any] = {
    require(args.length >= argsType.size)
    if (args.length == argsType.size) {
      if (argsType.last.isAssignableFrom(extractArgs(args)(args.length - 1).getClass)) {
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
}
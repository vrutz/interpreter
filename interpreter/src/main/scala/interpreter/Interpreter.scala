package interpreter

import scala.meta._

object Interpreter {

  def evaluate(terms: Seq[Term], env: Environment)(implicit ctx: Context): (Seq[Value], Environment) = {
    terms.foldRight(List[Value](), env) {
      case (expr, (evaluatedExprs, newEnv)) =>
        val (newEvaluatedExpr, envToKeep) = evaluate(expr, newEnv)
        (evaluatedExprs ::: List(newEvaluatedExpr), envToKeep)
    }
  }

  def evaluate(term: Term, env: Environment = new Environment())(implicit ctx: Context): (Value, Environment) = term match {
      /* Literals */
    case q"${x: Boolean}" => (Literal(x), env)
    case q"${x: Byte}" => (Literal(x), env)
    case q"${x: Short}" => (Literal(x), env)
    case q"${x: Int}" => (Literal(x), env)
    case q"${x: Long}" => (Literal(x), env)
    case q"${x: Float}" => (Literal(x), env)
    case q"${x: Double}" => (Literal(x), env)
    case q"${x: Char}" => (Literal(x), env)
    case q"${x: String}" => (Literal(x), env)
    case q"${x: scala.Symbol}" => (Literal(x), env)
    case q"null" => (Literal(null), env)
    case q"()" => (Literal(()), env)

      /* Expressions */
    // this: this | <expr>.this
    case q"this" => (env(This), env)
//    case q"${qname: Tree}.this" => evaluate(qname, env)

    // super: super | <expr>.super | super[<expr>] | <expr>.super[<expr]
//    case q"super" => (env(Super), env)
//    case q"${qname: Tree}.super" =>
//      val (instance: Instance, instanceEnv: Environment) = evaluate(qname, env)
//      (instanceEnv(Super), instanceEnv)
//    case q"super[${qname: Tree}]" =>
//      val (typeToApp: interpreter.Type, typeEnv) = evaluate(qname, env)
//      (env(Super), typeEnv)
//    case q"${qname0: Tree}.super[${qname1: Tree}]" =>
//      val (caller: Instance, callerEnv: Environment) = evaluate(qname0, env)
//      (callerEnv(Super), callerEnv)

//    case q"${name: Term.Name}" => (env(Local(name)), env)
      // Selection <expr>.<name>
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
      (Instance(t"List", Map[Slot,Value]()), env)
//    case q"$expr[..$tpes]" => ???
    case q"${expr: Term} ${name: Term.Name} (..$aexprs)" =>
      val (caller, callerEnv) = evaluate(expr, env)
      val paramss: Seq[Seq[Term.Param]] = name.defn match {
        case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" => paramss
      }
      println(paramss)
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
    case t => (Literal(null), env)
  }

	def find(toFind: String)(tree: Tree): Unit = tree.topDownBreak.collect {
    case t@q"$expr.$name" if (name: Term.Name).toString == toFind =>
      println(s"Found one $toFind in expression: $t")
      find(toFind)(expr)
    case t: Term.Name if t.toString == toFind =>
      println(s"Found one $toFind in expression: $t")
  }
}
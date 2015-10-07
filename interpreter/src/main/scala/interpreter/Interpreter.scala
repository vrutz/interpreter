package interpreter

import scala.meta._
import scala.meta.tql._

object Interpreter {

  def evaluate(terms: Seq[Tree], env: Environment)(implicit ctx: Context): (Seq[Value], Environment) = {
    (List[Value](), env)
  }

  def evaluate(term: Tree, env: Environment)(implicit ctx: Context): (Value, Environment) = term match {
    case q"${expr: Tree} ${name: Term.Name}[..$tpes] (..$aexprs)" =>
      val justArgExprs: Seq[Tree] = (aexprs: Seq[Term.Arg]) map {
        case arg"$name = $exp" => expr
        case arg"$exp: _*" => expr
        case arg"$exp" => expr
      }
      val (ev1,  env1) = evaluate(expr, env)
      val (ev2s: Seq[Value], env2: Environment) = evaluate(aexprs: Seq[Term.Arg], env1)
      val substExpr: Tree = name.defn match {
        case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" =>
          val p: Seq[(Term.Param.Name, Option[Tree])] = (paramss: Seq[Seq[Term.Param]]).flatten map {
              case param"..$mods $paramname: $atpeopt = $expropt" => (paramname: Term.Param.Name, expropt: Option[Tree])
            }

          val defParamsExprs: Seq[Tree] = p.unzip._2.takeRight(p.length - ev2s.length).map(_.get)

          val paramsWithExpr = (p.unzip._1 zip (justArgExprs ++ defParamsExprs)).toMap

          ((expr: Tree) transform {
              case name: Term.Name if paramsWithExpr.exists { case (n: Term.Param.Name, _) => n.toString == name.toString } =>
                paramsWithExpr(name)
              case q"this" => expr
            }).asInstanceOf[Tree]
      }
      // Replace type parameters in body of the function with tpes
      // Replace arguments in the body of the function with ev2s
      // Evaluate the transformed tree with env2

      (evaluate(substExpr, env2)._1, env2)
    case t => (Literal(null), env)
  }

	def find(toFind: String)(tree: Tree): Unit = tree.topDownBreak.collect {
		case t @ q"$expr.$name" if (name: Term.Name).toString == toFind =>
			println(s"Found one $toFind in expression: $t")
			find(toFind)(expr)
		case t: Term.Name if t.toString == toFind =>
			println(s"Found one $toFind in expression: $t")
	}
}
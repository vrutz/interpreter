package interpreter

import scala.meta._
import scala.meta.tql._

object Interpreter {

  def evaluate(terms: Seq[Tree], env: Environment)(implicit ctx: Context): (Seq[Value], Environment) = {
    (List[Value](), env)
  }

  def evaluate(term: Tree, env: Environment)(implicit ctx: Context): (Value, Environment) = term match {
    case q"$expr $name[..$tpes] (..$aexprs)" =>
      val (ev1,  env1) = evaluate(expr, env)
      val (ev2s: Seq[Value], env2: Environment) = evaluate(aexprs, env1)
      name.defn match {
        case q"..$mods def $name[..$tparams](...$paramss): $tpeopt = $expr" => println(paramss)
        case _ => println("Should not happen")
      }
      // Replace type parameters in body of the function with tpes
      // Replace arguments in the body of the function with ev2s
      // Evaluate the transformed tree with env2
      (???, env2)
    case t => (Literal(null), env)
  }

	def find(toFind: String)(tree: Tree): Unit = tree.topDownBreak.collect {
		case t @ q"$expr.$name" if name.toString == toFind =>
			println(s"Found one $toFind in expression: $t")
			find(toFind)(expr)
		case t: Term.Name if t.toString == toFind =>
			println(s"Found one $toFind in expression: $t")
	}
}
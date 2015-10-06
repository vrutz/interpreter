package interpreter

import scala.meta._
import scala.meta.semantic._
import scala.meta.tql._

object Interpreter {

  def evaluate(terms: Seq[Tree], env: Environment)(implicit ctx: Context): (Seq[Value], Environment) = {
    (List[Value](), env)
  }

  def evaluate(term: Tree, env: Environment)(implicit ctx: Context): (Value, Environment) = term match {
    case q"$expr $name[..$tpes] (..$aexprs)" =>
      val (ev1,  env1) = evaluate(expr, env)
      val (ev2s, env2) = evaluate(aexprs, env1)
      val infixDef = name.defn
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

	def main(args: Array[String]): Unit = {
     println("""
			|object O {
			|	def main(args: Array[String]) {
			|		1 + 2
			|	}
			|}
			""".stripMargin.parse[Stat].show[Structure])
	}
}
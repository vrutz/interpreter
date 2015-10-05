package interpreter

import scala.meta._
import scala.meta.semantic._
import scala.meta.tql._

object Interpreter {

//  def evaluate(term: Term, env: Environment)(implicit ctx: Context): (Value, Environment) = term match {
////    case q"$v1 + $v2" => (evaluate(v1, env)._1 + evaluate(v2, env)._1, env)
//    case name: Term.Name =>
//      val defn: Member.Term = name.defn
//      defn
//      (NullLit, env)
//    case t => (NullLit, env)
//  }

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
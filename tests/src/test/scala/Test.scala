import org.scalatest._
import scala.meta._
import scala.meta.tql._
import scala.meta.dialects.Scala211

class TestSuite extends FunSuite {
	test("foo") {
		def find(toFind: String)(tree: Tree): Unit = tree.collect {
			case t @ q"$expr.$name" if name.toString == toFind => println(s"Found one $toFind in expression: $t")
			case t: Term.Name if t.toString == toFind => println(s"Found one $toFind in expression: $t")
		}
/*
		tree \\: visit[Set] {
			case t @ q"$expr.$name" if name.toString == toFind => println(s"Found one get in expression: $t"); Set(expr)
			case t: Term.Name if t.toString == toFind => println(s"Found one get in expression: $t"); Set.empty
		} \\: feed { _.map(expr: Tree => find(toFind)(expr)) }
*/
		find("get")("""
			|object O {
			|	def main(args: Array[String]) {
			|		val x: Option[Option[Int]] = Some(Some(42))
			|		x.get.get
			|
			|		import x._
			|		get
			|	}
			|}
			""".stripMargin.parse[Stat])
	}
}
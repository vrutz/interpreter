import org.scalatest._
import scala.meta._
import scala.meta.tql._
import scala.meta.dialects.Scala211

class TestSuite extends FunSuite {
	test("foo") {
		def find(toFind: String)(tree: Tree): Unit = tree.topDownBreak.collect {
			case t @ q"$expr.$name" if name.toString == toFind =>
				println(s"Found one $toFind in expression: $t")
				find(toFind)(expr)
			case t: Term.Name if t.toString == toFind =>
				println(s"Found one $toFind in expression: $t")
		}
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
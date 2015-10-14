import org.scalatest.FunSuite

import scala.meta._
import scala.meta.dialects.Scala211
import interpreter.Interpreter._
/**
 * Created by rutz on 06/10/15.
 */
class TestEvaluate extends FunSuite {
  val scalaLibrary = sys.props("sbt.paths.scalalibrary.classes")
  val classpath = sys.props("sbt.paths.scrutinee.classes")
  val sourcepath = sys.props("sbt.paths.scrutinee.sources")

  implicit val c: Context = Context(Artifact(scalaLibrary))

  test("literal") {
    println(evaluate(q"{val x = 2; 0 + x}")._1)
  }
}

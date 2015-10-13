import org.scalatest.FunSuite

import scala.meta._
import scala.meta.dialects.Scala211
import interpreter._
import interpreter.Interpreter._

/**
 * Created by rutz on 06/10/15.
 */
class TestEvaluate extends FunSuite {
  val classpath = sys.props("sbt.paths.scrutinee.classes")
  val sourcepath = sys.props("sbt.paths.scrutinee.sources")

  implicit val c: Context = Context(Artifact(classpath, sourcepath))

  test("literal") {
    c.sources.foreach(s => println(evaluate(q"0", new Environment())._1))
  }
}

/**
 * Created by rutz on 05/10/15.
 */

import interpreter._
import org.scalatest._

class TestValue extends FunSuite {
  test("toString int") {
    assert(IntLit(0).toString == "0")
  }

  test("toString long") {
    assert(LongLit(0l).toString == "0")
  }
}
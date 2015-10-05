/**
 * Created by rutz on 05/10/15.
 */

import interpreter._
import org.scalatest._

class TestValue extends FunSuite {

  test("toString boolean") {
    assert(BooleanLit(true).toString == "true")
  }

  test("toString char") {
    assert(CharLit('a').toString == "\'a\'")
  }

  test("toString byte") {
    assert(ByteLit(0.toByte).toString == "0")
  }

  test("toString short") {
    assert(ShortLit(0).toString == "0")
  }

  test("toString int") {
    assert(IntLit(0).toString == "0")
  }

  test("toString long") {
    assert(LongLit(0l).toString == "0")
  }

  test("toString float") {
    assert(FloatLit(0.0f).toString == "0.0")
  }

  test("toString double") {
    assert(DoubleLit(0.0d).toString == "0.0")
  }

  test("toString String") {
    assert(StringLit("Hello world").toString == "\"Hello world\"")
  }

  test("toString null") {
    assert(NullLit.toString == "null")
  }

  test("toString unit") {
    assert(UnitLit.toString == "()")
  }
}
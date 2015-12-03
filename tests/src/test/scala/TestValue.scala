/**
 * Created by rutz on 05/10/15.
 */
package scala.meta
import interpreter._
import org.scalatest._
import internal.interpreter._
import internal.representations.Val

class TestValue extends FunSuite {

  test("toString boolean") {
    assert(Val(true).toString == "true")
  }

  test("toString char") {
    assert(Val('a').toString == "\'a\'")
  }

  test("toString byte") {
    assert(Val(0.toByte).toString == "0")
  }

  test("toString short") {
    assert(Val(0).toString == "0")
  }

  test("toString int") {
    assert(Val(0).toString == "0")
  }

  test("toString long") {
    assert(Val(0l).toString == "0")
  }

  test("toString float") {
    assert(Val(0.0f).toString == "0.0")
  }

  test("toString double") {
    assert(Val(0.0d).toString == "0.0")
  }

  test("toString String") {
    assert(Val("Hello world").toString == "\"Hello world\"")
  }

  test("toString null") {
    assert(Val(null).toString == "null")
  }

  test("toString unit") {
    assert(Val(()).toString == "()")
  }
}
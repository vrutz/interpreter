/**
 * Created by rutz on 05/10/15.
 */
package scala.meta
import interpreter._
import org.scalatest._
import internal.interpreter._
import internal.representations.Literal

class TestValue extends FunSuite {

  test("toString boolean") {
    assert(Literal(true).toString == "true")
  }

  test("toString char") {
    assert(Literal('a').toString == "\'a\'")
  }

  test("toString byte") {
    assert(Literal(0.toByte).toString == "0")
  }

  test("toString short") {
    assert(Literal(0).toString == "0")
  }

  test("toString int") {
    assert(Literal(0).toString == "0")
  }

  test("toString long") {
    assert(Literal(0l).toString == "0")
  }

  test("toString float") {
    assert(Literal(0.0f).toString == "0.0")
  }

  test("toString double") {
    assert(Literal(0.0d).toString == "0.0")
  }

  test("toString String") {
    assert(Literal("Hello world").toString == "\"Hello world\"")
  }

  test("toString null") {
    assert(Literal(null).toString == "null")
  }

  test("toString unit") {
    assert(Literal(()).toString == "()")
  }
}
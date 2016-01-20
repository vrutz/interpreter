package scala.meta

import org.scalatest.FunSuite

import tql._
import interpreter._
import internal.interpreter._
import internal.interpreter.Interpreter._

import internal.representations._

/**
 * Created by rutz on 06/10/15.
 */

class TestPerf extends FunSuite {
  val scala = sys.props("sbt.paths.scalalibrary.classes")
  val classes = "/Users/rutz/Developer/interpreter/scrutinee/target/scala-2.10/classes"

  test("quicksort compiled") {
    implicit val c: Context = Context(Artifact(scala+":"+classes))
     val Val(res: Array[Int]) = eval(q"""{ val x = Array(2, 1, 3, 4); test.Test.quicksort(x); x }""")
     assert(res.zip(Array(1, 2, 3, 4)) forall {
        case (a, b) => a == b 
      })
  }

  implicit val c: Context = Context(Artifact(scala))

  test("quicksort implementation") {
    val Val(res: Array[Int]) = eval("""
      |{
      | val x = Array(2, 1, 4, 3)
      |
      | def sort(xs: Array[Int]) = {
      |   def swap(i: Int, j: Int) {
      |     val t = xs(i)
      |     xs(i) = xs.apply(j)
      |     xs(j) = t
      |   }
      |
      |   def sort1(l: Int, r: Int): Unit = {
      |     val pivot = xs((l + r) / 2)
      |     var i = l
      |     var j = r
      |     while (i <= j) {
      |       while (xs(i) < pivot) i = i + 1
      |       while (xs(j) > pivot) j = j - 1
      |
      |       if (i <= j) {
      |         swap(i, j)
      |         i = i + 1
      |         j = j - 1
      |       }
      |     }
      |     if (l < j) sort1(l, j)
      |     if (j < r) sort1(i, r)
      |   }
      |
      |   sort1(0, xs.length - 1)
      | }
      |
      | sort(x)
      | x
      |}""".stripMargin.parse[Term])

      assert(res.zip(Array(1, 2, 3, 4)) forall {
        case(a, b) => a == b
        })
  }
}
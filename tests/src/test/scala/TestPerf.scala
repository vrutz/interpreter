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
  val sources = sys.props("sbt.paths.scrutinee.sources")
  val classes = sys.props("sbt.paths.scrutinee.classes")


  // test("quicksort compiled") {
  //   implicit val c: Context = Context(Artifact(scala+":"+classes))
  //    val Val(res: Array[Int]) = eval(q"""{ val x = Array(2, 1, 3, 4); O.quicksort(x); x }""")
  //    assert(res.zip(Array(1, 2, 3, 4)) forall {
  //       case (a, b) => a == b 
  //     })
  // }
}
package interpreter

/**
 * Created by rutz on 05/10/15.
 */
import scala.meta._

final class Environment(stack: CallStack) {
  def this() = this(List())

  def apply(name: Term.Name): Value = stack.head(name)
}
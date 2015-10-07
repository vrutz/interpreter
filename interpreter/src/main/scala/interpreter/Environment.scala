package interpreter

/**
 * Created by rutz on 05/10/15.
 */


final class Environment(stack: CallStack) {
  def this() = this(List())
}//, objects: TopLevelObjects)
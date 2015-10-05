/**
 * Created by rutz on 05/10/15.
 */

import scala.meta.Term

package object interpreter {
  type TopLevelObjects = Map[Term.Name, Value]
  type Frame = Map[Term.Name, Value]
  type CallStack = List[Frame]
}

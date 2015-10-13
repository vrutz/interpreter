/**
 * Created by rutz on 05/10/15.
 */


package object interpreter {
//  type TopLevelObjects = Map[Term.Name, Value]
  type Frame = Map[Slot, Value]
  type CallStack = List[Frame]
}

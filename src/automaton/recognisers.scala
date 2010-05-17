package marbles
package automaton

import util._

trait TreeAutomaton[T,R] 
	extends PartialFunction[OrderedTree[T],R]

abstract class DFTA[T] extends TreeAutomaton[T,Boolean] {
	
}

abstract class WFTA[T,R >: Semiring] extends TreeAutomaton[T,R] {
}



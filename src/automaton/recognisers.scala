package marbles.automaton
import marbles.util._


trait TreeAutomaton[T,R] 
	extends PartialFunction[Tree[T],R]

abstract class NFTA[T] extends TreeAutomaton[T,Boolean]




abstract class WFTA[T,R <% Semiring[R]] extends TreeAutomaton[T,R] {

}



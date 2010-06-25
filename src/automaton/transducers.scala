package marbles.automaton
import marbles.util._


/** A TreeTransducer from F to T is a (potentially multivalued) function
 *  from trees over F to trees over T
 */
trait TreeTransducer[F,T] {
	def apply(a:Tree[F]):Set[Tree[T]]
//	def processTree(a:OrderedTree[F]):Unit
//	def singleStep():Boolean
//	def parallelStep():Boolean
//	def currentTree:Tree[Any]
}

trait WTreeTransducer[F,T,R <: Semiring[R] {
	def apply(a:Tree[F]):Set[(Tree[T],R)]
}


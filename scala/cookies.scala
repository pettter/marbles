class Tree[T] (rt : T, ss: Collection[Tree[T]]) { 
	val root = rt
	val subs = ss
}              

class Alphabet[T] extends scala.collection.immutable.HashSet[T] {
	def verifyTree(t: Tree[T]):Boolean = 
		this.contains(t.root) && t.subs.forall(verifyTree _)
}

class RankedAlphabet[T] extends scala.collection.immutable.HashMap[T,Int] {
	def verifyTree(t: Tree[T]):Boolean = 
		this.isDefinedAt(t.root) && t.subs.size == this(t.root) &&
			t.subs.forall(verifyTree _)
}

class OrderedTree[T](rt:T,ss:Seq[Tree[T]]) extends Tree[T](rt,ss) 

trait TreeTransducer[F,T]
   	   extends PartialFunction[Tree[F],Tree[T]]

class TDTreeTransducer[F,T](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String],
	   val rules:Map[String,Tree[Either[(String,Tree[F]),T]]]
	   val q0:String)
	   extends TreeTransducer[F,T]
{
	private def applyState(t : Tree[Either[(String,Tree[F]),T]]]):Tree[T] = 
		match t.root {
		case Left((q,input)) =>
		case Right(root) => new Tree[T](root,t.subs map (applyState _))
	}

	def apply(t:Tree[F]):Tree[T] = applyState(Left((q0,t)))
}



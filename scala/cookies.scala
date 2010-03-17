class Tree[T] (rt : T, ss: Traversable[Tree[T]]) { 
	val root = rt
	val subtrees = ss

	def subst(sym : T,sub : Tree[T]) : Tree[T] = {
		subst(Map[T,Tree[T]]((sym ,sub)))
	}

	def subst(subs : Map[T,Tree[T]]) : Tree[T] = {
		if(subs isDefinedAt root)
			subs(root)
		else
			new Tree[T](root,subtrees map(_.subst(subs)) toList)
	}

	override def toString : String = subtrees.size match { 
		case 0 => root toString
		case _   => root.toString + subtrees.mkString("[",",","]") 
	}

	override def equals(other : Any) = other match{
		case that : Tree[_] => this.root == that.root &&
							   this.subtrees == that.subtrees
		case _ => false
	}

	override def hashCode = 41 * ( 41 + root.hashCode) + subtrees.hashCode
}

class Alphabet[T] extends scala.collection.immutable.HashSet[T] {
	def verifyTree(t: Tree[T]):Boolean = 
		this.contains(t.root) && 
		t.subtrees.forall(verifyTree _)
}

class RankedAlphabet[T] extends scala.collection.immutable.HashMap[T,Int] {
	def verifyTree(t: Tree[T]):Boolean = 
		this.isDefinedAt(t.root) && 
		t.subtrees.size == this(t.root) &&
		t.subtrees.forall(verifyTree _)
}

class OrderedTree[T](rt:T,ss:Seq[OrderedTree[T]]) extends Tree[T](rt,ss) {
	override val subtrees:Seq[OrderedTree[T]] = ss
}

trait TreeTransducer[F,T]
   	   extends PartialFunction[OrderedTree[F],OrderedTree[T]]

class TDTreeTransducer[F,T](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(String,F),(T,List[(String,Int)])],
	   val q0:String)
	   extends TreeTransducer[F,T]
{
	private def applyState(q : String,t : OrderedTree[F]):OrderedTree[T] ={
		val (troot,sts) = rules(q,t.root)
		new OrderedTree[T](troot,sts.map(s =>
					applyState(s._1,t.subtrees(s._2))))
	}

	def apply(t:OrderedTree[F]):OrderedTree[T] = applyState(q0,t)

	def isDefinedAt( t : OrderedTree[F]):Boolean = {
		try{
			(applyState(q0,t))
			true
		}catch{
			case ex:MatchError => false
		}
	}
}




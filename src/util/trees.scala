package marbles
package util

// XXX XXX XXX XXX
// TODO: IMPORTANT!
// These classes MUST be split up and 'traitified', similar to how the
// collections framework is done in the Scala standard library. However,
// check compatibility with Java as well, since these will in many cases be
// used there.
// XXX XXX XXX XXX

class Tree[+T] (rt : T, ss: Traversable[Tree[T]]) { 
	val root = rt
	val subtrees = ss

	def subst[U >: T](sym : U,sub : Tree[U]) : Tree[U] = {
		subst(Map[U,Tree[U]]((sym ,sub)))
	}

	def subst[U >: T](subs : Map[U,Tree[U]]) : Tree[U] = {
		if(subs isDefinedAt root)
			subs(root)
		else
			new Tree[U](root,subtrees map(_.subst(subs)) toList)
	}

	def map[To](f : (T) => (To)):Tree[To] = {
		new Tree(f(rt), ss map (_ map f))
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

class MutableTree[T](rt : T, ss: Traversable[Tree[T]]) { //extends Tree[T](rt,ss) {
	var root = rt;
	val subtrees = ss;
}

class OrderedTree[T](rt:T,ss:Seq[OrderedTree[T]]) extends Tree[T](rt,ss) {
	override val subtrees:Seq[OrderedTree[T]] = ss
	
	def leaves:Seq[OrderedTree[T]] = if(subtrees.size == 0)
										this :: Nil 
									else
										subtrees flatMap (_.leaves)
}

class WeightedTree[T,V](rt:Tuple2[T,V],
						ss:Traversable[WeightedTree[T,V]]) extends 
							Tree[Tuple2[T,V]](rt,ss) {
	
}


class TreeWithWeight[T,V](t:Tree[T],v:V) {
	val tree = t;
	val value  = v;
}

class VarTree[T](rt:Either[Variable,T], ss:Traversable[VarTree[T]]) extends 
		Tree[Either[Variable,T]](rt,ss){
	
	override val subtrees:Traversable[VarTree[T]] = ss

	def subAll(ts:Seq[Tree[T]]):Tree[T] = root match {
			case Left(Variable(ix)) => ts(ix)
			case Right(r) => new Tree(r,(subtrees map (_.subAll(ts))))
		}

	def occurrences:Map[Variable,Int] = root match{
		case Left(v) => Map((v,1))

		case _ => { 
			val maps = (subtrees map (_.occurrences)) 
			(for(k:Variable <- (maps flatMap (_.keys) toSet)) yield 
			(k,(maps map(_.getOrElse(k,0)) sum))) toMap
		}
	}


}

object Util {
	def cartSet[T](sets: Seq[Set[T]]): Set[Seq[T]] = {
		sets.toList match {
			case Nil => Set(Seq())
			case x :: xs => 
				val l = cartSet(xs)
				x.flatMap(e => l.map(f => e +: f))
		}
	}
}


object OrderedVarTree {
	def apply[T](rt:Either[Variable,T], ss:Seq[OrderedVarTree[T]]) = 
			new OrderedVarTree[T](rt,ss)
	
	def apply[T](rt:T,r:Int) = rootWithVars(rt,r)
	
	def rootWithVars[T](rt:T,r:Int) = 
			new OrderedVarTree(
					Right(rt),
					for(ix <- 0 until r) yield
						new OrderedVarTree(
							Left(Variable(ix)):Either[Variable,T],
							Nil
						)
				)
}

class OrderedVarTree[T](rt:Either[Variable,T], ss:Seq[OrderedVarTree[T]]) extends 
		OrderedTree[Either[Variable,T]](rt,ss){
	override val subtrees:Seq[OrderedVarTree[T]] = ss

	override def toString:String = ((root match {
		case Left(x) => x toString
		case Right(x) => x toString 
	}) + (subtrees.size match {
		case 0 => ""
		case _   => "" + subtrees.mkString("[",",","]") 
	}))


	def subAll(ts:Seq[OrderedTree[T]]):OrderedTree[T] = root match {
			case Left(Variable(ix)) => ts(ix)
			case Right(r) => new OrderedTree(r,(subtrees map (_.subAll(ts))))
		}
	
	def subst[U >: T](subs : Map[Either[Variable,U],OrderedVarTree[U]]) :
		OrderedVarTree[U] = {
		if(subs isDefinedAt root)
			subs(root)
		else
			new OrderedVarTree[U](root,subtrees map(_.subst(subs)) toList)
	}
	
//	def subAll(ts:Seq[Set[OrderedTree[T]]]):Set[OrderedTree[T]] = root match {
//			case Left(Variable(ix)) => ts(ix)
//			case Right(r) => for(seq <- Util.cartSet(subtrees map (_.subAll(ts)))) yield
//				new OrderedTree(r,seq)
//		}
	
	def rank:Int =  ((root match {
			case Left(Variable(i)) => i+1
			case _ => 0 
		}) /: subtrees)(_ max _.rank)
	
	def occurrences:Map[Variable,Int] = root match{
		case Left(v) => Map((v,1))

		case _ => { 
			val maps = (subtrees map (_.occurrences)) 
			(for(k:Variable <- (maps flatMap (_.keys) toSet)) yield 
			(k,(maps map(_.getOrElse(k,0)) sum))) toMap
		}
	}
}


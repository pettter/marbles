package marbles
package util

import scala.util.parsing.combinator._
// XXX XXX XXX XXX
// TODO: IMPORTANT!
// These classes MUST be split up and 'traitified', similar to how the
// collections framework is done in the Scala standard library. However,
// check compatibility with Java as well, since these will in many cases be
// used there.
// XXX XXX XXX XXX
// CANCELED: This will take far too much time and effort which is better
// spent on other things. A Tree will refer to an ordered tree for now, and
// a VarTree to an ordered ditto. Refactoring to fit this will be easier
// than implementing a fullblown collections framework-style type cluster


/** Companion object for the Tree class. Contains factory and constructor
 *  methods, as well as extractors.
 */
object Tree {
	/** Standard factory method
	 */
	def apply[T](rt:T,ss:Seq[Tree[T]]) = new Tree(rt,ss)
	
	/** Factory for leaves
	 */
	def apply[T](rt:T) = new Tree(rt,Nil)
	
	/** A parser for trees on the form "root[tree,...]" or "root" if the
	 *  tree is a leaf. 
	 */
	implicit def treeParser[T](implicit rootParsers:ElementParsers[T]) = new ElementParsers[Tree[T]] {
		val root:Parser[T] = rootParsers//.start.asInstanceOf[Parser[T]]
		def tree:Parser[Tree[T]] =
			(root~opt("["~>repsep(tree,",")<~"]")) ^^ {
				case root~None       => new Tree(root,Nil)
				case root~Some(subs) => new Tree(root,subs)
			}

		def start = tree
	}

	/** Extracts the root and subtrees of a tree.
	 */
	def unapply[T](t:Tree[T]):(T,Seq[Tree[T]]) = (t.root,t.subtrees)
	
}

/** Implementation traits are omitted for now
 */
//trait TreeLike[+T,+TT[_] >: Tree[_]] {
//
//	val root:T
//	val subtrees:Traversable[TT[T]]
//
//}

/** An ordered tree with nodes of a certain type
 */
class Tree[+T] (val root : T, 
		        val subtrees: Seq[Tree[T]]
			) { 

	/** Substitute all occurances of a certain symbol for a tree over the
	 *  same type (or a supertype) returns a tree over the new type
	 */
	def subst[U >: T](sym : U,sub : Tree[U]) : Tree[U] = {
		subst(Map[U,Tree[U]]((sym ,sub)))
	}

	/** Substitute all occurances of certain symbols for designated 
	 *  trees over the same type (or a supertype) returns a tree over the
	 *  new type
	 */
	def subst[U >: T](subs : Map[U,Tree[U]]) : Tree[U] = {
		if(subs isDefinedAt root)
			subs(root)
		else
			new Tree[U](root,subtrees map(_.subst(subs)) toList)
	}
	
	/** Apply a function to every node of this tree.
	 */
	def map[To](f : (T) => (To)):Tree[To] = {
		new Tree(f(root), subtrees map (_ map f))
	}
	
	/** Get the leaf trees of this tree
	 */
	def leaves:Seq[Tree[T]] = if(subtrees.size == 0)
										this :: Nil 
									else
										subtrees flatMap (_.leaves)

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

/** Unused
 */
class MutableTree[T](rt : T, ss: Traversable[Tree[T]]) { //extends Tree[T](rt,ss) {
	var root = rt;
	val subtrees = ss;
}

/** Unused
 */
class OrderedTree[T](rt:T,ss:Seq[OrderedTree[T]]) extends Tree[T](rt,ss) {
	override val subtrees:Seq[OrderedTree[T]] = ss
	
}

/** A tree where each node is associated with a weight
 */
class WeightedTree[T,V](rt:Tuple2[T,V],
						ss:Seq[WeightedTree[T,V]]) extends 
							Tree[(T,V)](rt,ss) {
	
}

/** A tree with associated weight
 */
class TreeWithWeight[T,V](t:Tree[T],v:V) {
	val tree = t;
	val value  = v;
}

//class VarTree[T](rt:Either[Variable,T], ss:Seq[VarTree[T]]) extends 
//		Tree[Either[Variable,T]](rt,ss){
//	
//	override val subtrees:Traversable[VarTree[T]] = ss
//
//	def subAll(ts:Seq[Tree[T]]):Tree[T] = root match {
//			case Left(Variable(ix)) => ts(ix)
//			case Right(r) => new Tree(r,(subtrees map (_.subAll(ts))))
//		}
//
//	def occurrences:Map[Variable,Int] = root match{
//		case Left(v) => Map((v,1))
//
//		case _ => { 
//			val maps = (subtrees map (_.occurrences)) 
//			(for(k:Variable <- (maps flatMap (_.keys) toSet)) yield 
//			(k,(maps map(_.getOrElse(k,0)) sum))) toMap
//		}
//	}
//
//}


/** Companion object of the VarTree class, containing factory methods and a
 *  parser.
 */
object VarTree {

	/** Standard factory method
	 */
	def apply[T](rt:Either[Variable,T], ss:Seq[VarTree[T]]) = 
			new VarTree[T](rt,ss)
	
	/** Create a VarTree with the designated root with, basically a range
	 *  of variables as subtrees.
	 */
	def apply[T](rt:T,r:Int) = rootWithVars(rt,r)

	/** Convert a tree of Either[Variable,T] into a VarTree[T]
	 */
	def apply[T](tree:Tree[Either[Variable,T]]):VarTree[T] =
		VarTree[T](tree.root,tree.subtrees map (VarTree(_)))
	
	/** Create a VarTree with the designated root with, basically a range
	 *  of variables as subtrees.
	 */
	def rootWithVars[T](rt:T,r:Int) = 
			new VarTree(
					Right(rt),
					for(ix <- 0 until r) yield
						new VarTree(
							Left(Variable(ix)):Either[Variable,T],
							Nil
						)
		)

	/** Create a parser for VarTree[T] where each node is either a T
	 *  (parsed with the supplied parser) or a variable on the form
	 *  "{integer}"
	 */
	def varTreeParser[T](implicit tps:ElementParsers[T]) = 
			new ElementParsers[VarTree[T]] {
				val tp:Parser[T] = tps
				val vp:Parser[Variable] = Variable.varParser
				val root:Parser[Either[Variable,T]] = either(vp,tp)
				def tree:Parser[VarTree[T]] =
					(root~opt("["~>repsep(tree,",")<~"]")) ^^ {
						case root~None       => new VarTree(root,Nil)
						case root~Some(subs) => new VarTree(root,subs)
					}

				def start = tree
			}
}

/** A VarTree is a tree over T and Variable.
 */
class VarTree[T](rt:Either[Variable,T], ss:Seq[VarTree[T]]) extends 
		Tree[Either[Variable,T]](rt,ss){
	override val subtrees:Seq[VarTree[T]] = ss

	override def toString:String = ((root match {
		case Left(x) => x toString
		case Right(x) => x toString 
	}) + (subtrees.size match {
		case 0 => ""
		case _   => "" + subtrees.mkString("[",",","]") 
	}))


	/** Replace each variable with the corresponding tree in the input
	 *  sequence, and return the resulting tree over T
	 */
	def subAll(ts:Seq[Tree[T]]):Tree[T] = root match {
			case Left(Variable(ix)) => ts(ix)
			case Right(r) => new Tree(r,(subtrees map (_.subAll(ts))))
		}
	
	/** Standard substitution, reimplemented for typing reasons from Tree
	 */
	def subst[U >: T](subs : Map[Either[Variable,U],VarTree[U]]) :
		VarTree[U] = {
		if(subs isDefinedAt root)
			subs(root)
		else
			new VarTree[U](root,subtrees map(_.subst(subs)) toList)
	}
	
//	def subAll(ts:Seq[Set[Tree[T]]]):Set[Tree[T]] = root match {
//			case Left(Variable(ix)) => ts(ix)
//			case Right(r) => for(seq <- Util.cartSet(subtrees map (_.subAll(ts)))) yield
//				new Tree(r,seq)
//		}
	/** The 'rank' of the VarTree is the highest number of any variable,
	 *  i.e. the lowest number of trees in a working input sequence for
	 *  subAll
	 */
	def rank:Int =  ((root match {
			case Left(Variable(i)) => i+1
			case _ => 0 
		}) /: subtrees)(_ max _.rank)
	
	/** Gives a map from each variable to the number of occurrences of that
	 *  variable in the tree.
	 */
	def occurrences:Map[Variable,Int] = root match{
		case Left(v) => Map((v,1))

		case _ => { 
			val maps = (subtrees map (_.occurrences)) 
			(for(k:Variable <- (maps flatMap (_.keys) toSet)) yield 
			(k,(maps map(_.getOrElse(k,0)) sum))) toMap
		}
	}
}


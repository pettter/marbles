package marbles.automaton
import marbles.util._

import scala.collection.Map
import scala.collection.immutable.Set

trait TreeTransducer[F,T] {
	def apply(a:OrderedTree[F]):Set[OrderedTree[T]]
//	def processTree(a:OrderedTree[F]):Unit
//	def singleStep():Boolean
//	def parallelStep():Boolean
//	def currentTree:Tree[Any]
}


class TDTreeTransducer[F,T](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(F,String),Set[(OrderedVarTree[T],Seq[(String,Int)])]],
	   val q0:Set[String]) extends AnyRef with TreeTransducer[F,T] {
	
	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma         : "+sigma+
		"\nDelta         : "+delta+
		"\nStates        : "+states+
		"\nInitial states: "+q0+
		"\nRules         :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" => "+rhs+"\n")}
		ret.toString
	}

	private def applyState(q : String,tree : OrderedTree[F]):Set[OrderedTree[T]] ={
		val rhs = rules(tree.root,q)
		for(
			 (t,pairs) <- rhs; // Pairs of index/state to process and
			 				   // insert into the output tree t
			 (treeseq) <- Util.cartSet(
				 			(pairs map 
							 	{ case (st,ix) =>
					 				(st,tree.subtrees(ix)) 
					 			}) map (x => applyState(x._1,x._2))
							)
		   ) yield
			t.subAll(treeseq)
//				(sts zip tree.subtrees) map 
//				(n => applyState(n._1,n._2))
//			)
//		) flatten
	}

	def apply(t:OrderedTree[F]):Set[OrderedTree[T]] = //try{
		//	Some(
			(for(q <- q0) yield applyState(q,t)).flatten
		
		//} catch {
		//	case ex:MatchError => None
		//}

	def isDefinedAt( t : OrderedTree[F]):Boolean = {
		apply(t).nonEmpty
//		try{
//			(applyState(q0,t))
//			true
//		}catch{
//			case ex:MatchError => false
//		}
	}
}

class BUTreeTransducer[F,T](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(F,Seq[String]),Set[(OrderedVarTree[T],String)]],
	   val fin:Set[String]) extends TreeTransducer[F,T] {


	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma       : "+sigma+
		"\nDelta       : "+delta+
		"\nStates      : "+states+
		"\nFinal states: "+fin+
		"\nRules       :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" => "+rhs+"\n")}
		ret.toString
	}



	def applyState(t : OrderedTree[F]):Set[(OrderedTree[T],String)] = {
		val pairs = Util.cartSet(t.subtrees map applyState)
		(for((trees,states) <- pairs map (_.unzip) if rules.isDefinedAt(t.root,states) ;
			(vtree,state) <- rules(t.root,states)) yield 
				(vtree subAll trees,state)) toSet
//		(vtree subAll trees,state)
	}


	def apply( tree :OrderedTree[F] ):Set[OrderedTree[T]] = {
		(for((t,s) <- applyState(tree) if fin contains s) yield t) toSet
//			return if (fin intersection s).nonEmpty
//					Some(t)
//				else
//					None
	}

	def isDefinedAt( t : OrderedTree[F]):Boolean = {
		(apply(t).nonEmpty)
	}

}



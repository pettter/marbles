package marbles.algorithm

import marbles.util._
import marbles.automaton.BUTreeTransducer
import marbles.automaton.TDTreeTransducer

/** An algorithm for splitting a Bottom-Up Tree Transducer into two
 *  Top-Down Tree Transducers which implement the same function.
 */ 

class BUTSplitter[F,T](val but:BUTreeTransducer[F,T]) {

	// The alphabet of right-hand sides of the bu transducer
	private val treeAlpha = (new RankedAlphabet(
					(for(rhs <- but.rules.values; 
						v <- rhs) 
					yield (v._1, v._1.rank)) toMap)
				)

	val rel:TDTreeTransducer[F,VarTree[T]] = //Relabeling
		new TDTreeTransducer(
				but.sigma, // Same input alphabet obviously
				treeAlpha, // Right hand sides as 'state markers'
				but.states, // Same states in the relabeling`
				// For-loop below creates a sequence of pairs which fit
				// into a TD transducer, however the right hand sides still
				// need to be organised into sets to make a proper map
				(for(((sym,states), pairs) <- but.rules.toList;  
					( tree       , state) <- pairs) yield 
					(
						(sym,state),
						// Note: tree is the root of a height-1 tree
						(VarTree(tree,tree.rank),states.zipWithIndex)
					)) groupBy (_._1) map ({case (lhs,rhss) => 
						(lhs,(rhss map(_._2)) toSet)}) , 
				but.fin	// Final states is equivalent to initial states	
			)
	val hom:TDTreeTransducer[VarTree[T],T] = //Homomorphism
		new TDTreeTransducer(
				treeAlpha, // Output from relabeling is input for this
				but.delta, // While output is output from the original
				Set("q"),  // Only a single state needed for a homomorphism
				(for((tree,rank) <- treeAlpha.map) yield
				 ((tree,"q"),
				  Set((tree,Seq.fill(rank)("q") zipWithIndex)))) toMap,
				Set("q")   // The single state is also initial
				)

	val output = (rel,hom)
	
	def apply():(marbles.automaton.TDTreeTransducer[F,VarTree[T]],TDTreeTransducer[VarTree[T],T]) = (rel,hom);
	
}

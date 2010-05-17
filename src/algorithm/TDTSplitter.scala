package marbles.algorithm

import marbles.util._
import marbles.automaton._
//import marbles.automaton.TDTreeTransducer
//import marbles.automaton.BUTreeTransducer

class TDTSplitter[F,T](val tdt:TDTreeTransducer[F,T]) {
	
	val maxCopy = ((for(
			rhss <- tdt.rules.values;
			(_,stsixs) <- rhss) yield
		(stsixs groupBy(_._2)) map (_._2.size)) flatten) max

	val nSigma = tdt.sigma multiRanked(maxCopy)


	// A simple homomorphism to preemptively 'explode' the trees into
	// as many subtrees as are potentially needed for copying. Actually
	// more, since the global max is used instead of minimizing on the
	// level of each symbol
	val hom = new BUTreeTransducer[F,F](
		tdt.sigma, // Same input alphabet, obviously
		nSigma, // Output is in the 'exploded' alphabet
		Set("q"), // A single state is sufficient for homomorphisms
		// Rules copy each subtree maxCopy times, 'exploding' the tree
		(for(sym:F <- tdt.sigma.map.keys) yield (
			(sym,Seq.fill(nSigma(sym))("q")),
			Set((new OrderedVarTree(Right(sym),(
				(for(i <- 0 until tdt.sigma(sym);
					j <- 0 until maxCopy) yield
					new OrderedVarTree[F](Left(Variable(i)),Nil)))),
			 "q")))) toMap,
		Set("q") // a single, final state
	)

	private def mangleIndices(stsixs:Seq[(String,Int)],
			      ixmap:Map[Int,Int],
				  tix:Int
				 ):List[(Int,String,Int)] =
		stsixs.headOption match {
			case Some((s,x)) => {
					val occ = ixmap.getOrElse(x,0)
					(tix,s,x*maxCopy + occ) ::
						mangleIndices(stsixs.tail,ixmap.updated(x,occ + 1),tix + 1)
			}
			case None => Nil 
		}

	private def mangleIndices(ixs:Seq[(String,Int)]):(Map[
			 		Either[Variable,T],
					OrderedVarTree[T]],
				Map[Int,String]) = {
		val tuples = mangleIndices(ixs,Map(),0)
		val (varmap,statemap) = (tuples map { 
			case (oldindex,state,newindex) => (
				(Left(Variable(oldindex)),
				 OrderedVarTree(Left(Variable(newindex)),Nil):OrderedVarTree[T]
				),
				(newindex,state)
			)
		}) unzip;
		((varmap toMap),statemap toMap)
	}
		
	val lbseq = (for(((sym,state), pairs ) <- tdt.rules.toList;// Split up each rule
			 ( tree      , stsixs) <- pairs) yield {
		 // What we try to do here is threefold, really.
		 // 1. We try to translate the original TD rules to a form linear
		 //    in the new alphabet,
		 // 2. We try to extract the information about these intermediate
		 //    TD rules in a reasonable form, and
		 // 3. We try to translate the thus extracted information into
		 //    suitable linear BU rules
		 //
		 // Several things make this difficult.
		 // * The index/state pairs of the original rules refer to the
		 //   subtrees of the original trees, which are in tdt.sigma, not
		 //   in nSigma
		 // * We need to create a sequence of tuples instead of a map, and
		 //   then use groupBy, map and flatten to get the proper rule map,
		 //   since certain symbol/statesequence combinations may crop up
		 //   in more than one symbol/state pair.
		 // * The indices of the variables in the trees must be substituted
		 //   for the correct ones (the map for doing this is contructed in
		 //   the mangleIndices function)
			val (varmap,statemap) = mangleIndices(stsixs)
			val t = tree.subst(varmap)
			val states = for(ix <- 0 until nSigma(sym)) yield
							statemap.getOrElse(ix,"")
			((sym,states),(t,state))})

	// A linear bottom-up tree transducer, equivalent to the top-down tree
	// transducer given in the J. Engelfriet lecture notes from 1974.
	// In principle, it takes the exploded trees and uses as many copies of
	// each subtree as is necessary to achieve the same result as the
	// original TDT
	val lb = new BUTreeTransducer[F,T](
		nSigma, // exploded input alphabet
		tdt.delta, // original output alphabet
		tdt.states + "", // Original states, except that we need a
						 // 'dead' state to make sure that all subtrees
						 // are live in some sense. I.e. execution will not
						 // stop just because an irrelevant subtree is dead
		// As always, the rule translation is the most hairy part
		((for((sym,rank) <- nSigma.map) yield // First, the 'dead' state
		 ((sym,Seq.fill(rank)("")),
		  // We need to supply a tree, but what tree does not really
		  // matter. In the lecture notes, sigma is part of the new delta,
		  // and the 'dead' state applies the idenity. Here, however, delta
		  // and sigma may not be of the same type, meaning that making a
		  // single leaf node from delta is better. These trees will never
		  // show up in the end result of the transducer.
		  (OrderedVarTree(tdt.delta.leaves.head,0),""))).toList ++
		// Now for the proper rules 
		(for(((sym,state), pairs ) <- tdt.rules.toList;// Split up each rule
			 ( tree      , stsixs) <- pairs) yield {
		 // What we try to do here is threefold, really.
		 // 1. We try to translate the original TD rules to a form linear
		 //    in the new alphabet,
		 // 2. We try to extract the information about these intermediate
		 //    TD rules in a reasonable form, and
		 // 3. We try to translate the thus extracted information into
		 //    suitable linear BU rules
		 //
		 // Several things make this difficult.
		 // * The index/state pairs of the original rules refer to the
		 //   subtrees of the original trees, which are in tdt.sigma, not
		 //   in nSigma
		 // * We need to create a sequence of tuples instead of a map, and
		 //   then use groupBy, map and flatten to get the proper rule map,
		 //   since certain symbol/statesequence combinations may crop up
		 //   in more than one symbol/state pair.
		 // * The indices of the variables in the trees must be substituted
		 //   for the correct ones (the map for doing this is contructed in
		 //   the mangleIndices function)
			val (varmap,statemap) = mangleIndices(stsixs)
			val t = tree.subst(varmap)
			val states = for(ix <- 0 until nSigma(sym)) yield
							statemap.getOrElse(ix,"")
			((sym,states),(t,state))
		})) groupBy (_._1) map ({ case (lhs,rhss) =>
   			(lhs,(rhss map (_._2)) toSet)}),
		tdt.q0
	)

//	val rel:TDTreeTransducer[F,OrderedVarTree[T]] = //Relabeling
//		new TDTreeTransducer(
//				but.sigma, // Same input alphabet obviously
//				treeAlpha, // Right hand sides as 'state markers'
//				but.states, // Same states in the relabeling`
//				// For-loop below creates a sequence of pairs which fit
//				// into a TD transducer, however the right hand sides still
//				// need to be organised into sets to make a proper map
//				(for(((sym,states), pairs) <- but.rules.toList;  
//					( tree       , state) <- pairs) yield 
//					(
//						(sym,state),
//						// Note: tree is the root of a height-1 tree
//						(OrderedVarTree(tree,tree.rank),states)
//					)) groupBy (_._1) map ({case (lhs,rhss) => 
//						(lhs,(rhss map(_._2)) toSet)}),
//				but.fin	// Final states is equivalent to initial states	
//			)
//	val hom:TDTreeTransducer[OrderedVarTree[T],T] = //Homomorphism
//		new TDTreeTransducer(
//				treeAlpha, // Output from relabeling is input for this
//				but.delta, // While output is output from the original
//				Set("q"),  // Only a single state needed for a homomorphism
//				(for((tree,rank) <- treeAlpha.map) yield
//				 ((tree,"q"),
//				  Set((tree,Seq.fill(rank)("q"))))) toMap,
//				Set("q")   // The single state is also initial
//				)
//	
//	val output = (hom,rel)
//
//	def apply():(marbles.automaton.TDTreeTransducer[F,OrderedVarTree[T]],TDTreeTransducer[OrderedVarTree[T],T]) = (rel,hom);
	
}

	//var input:TDTreeTransducer = Null

	//def output:(BUTreeTranducer[F,OrderedVarTree[T]],BUTreeTransducer[OrderedVarTree[T],T]) = null

package marbles.automaton
import marbles.util._

/** Companion object of the Top-Down Tree Transducer class, containing
 *  parsers and factory methods.
 */
object TDTreeTransducer {
	/** A parser for top-down tree transducers
	 */
	def TDTTParsers[F,T](implicit fps:ElementParsers[F],
			                      tps:ElementParsers[T]) = 
		new ElementParsers[TDTreeTransducer[F,T]] {
			// Parser for the input alphabet
			val tp:Parser[T] = tps
			// Parser for the ouput alphabet
			val fp:Parser[F] = fps
			// Parser for variable trees
			val vp:Parser[VarTree[T]] = VarTree.varTreeParser[T](tps)
			// A parser for alphabets over F, basically, the input alphabet
			def alpha:Parser[RankedAlphabet[F]] = 
				"{"~>RankedAlphabet.alphaParsers[F](fps)<~"}"
			// A parser for alphabets over T, basically, the output alphabet
			def beta:Parser[RankedAlphabet[T]] = 
				"{"~>RankedAlphabet.alphaParsers[T](tps)<~"}"
			// A state is a string
			def state:Parser[String] = nonTreeParser
			// Parser for a set of states
			def states:Parser[Set[String]] = 
					"{"~>repsep(state,",")<~"}" ^^ (_.toSet)
			// A left-hand side is an input symbol and a state
			def lhs:Parser[(F,String)] = fp~"|"~state ^^ {case sym~"|"~st => (sym,st)}
			
			// The right-hand side is parsed from a tree of output symbols
			// and state-variable pairs
			def rhstree:Parser[Tree[Either[(String,Int),T]]] = (
				  tp~opt("["~>repsep(rhstree,",")<~"]") ^^ {
					  case root~None       => new Tree(Right(root),Nil)
					  case root~Some(subs) => new Tree(Right(root),subs)
				  }
				| "{"~>(state~"|"~intParser)<~"}" ^^ {
					  case st~"|"~no       => new Tree(Left((st,no)),Nil)
				  }
				)
				
			// This is basically a transformation from the trees
			// constructed in the above parser to the tree-sequence pairs
			// used in the actual transducer
			def rhs:Parser[(VarTree[T],Seq[(String, Int)])] = rhstree ^^ ( rht => {
				val tuples:Seq[Either[(String,Int),T]] = 
					(rht.leaves filter (_.root.isLeft)).toSet.toList map (
						(x:Tree[Either[(String,Int),T]]) => x.root)

				val vtree = VarTree((rht subst ((
					tuples zip (tuples.indices map (x => new
							Tree(Left(("",x)),Nil)))
					) toMap)) map {
						case Left((_,ix)) => Left(Variable(ix))
						case Right(x) => Right(x)
					})
				(vtree,tuples collect {
				 	case Left(x) => x
					})
				}


				
			)
			// A parser is a left-hand side and a right-hand side
			def rule:Parser[((F,String),(VarTree[T],Seq[(String, Int)]))] = 
				lhs~"|"~rhs ^^ {case lh~"|"~rh => (lh,rh)}
			// We need to gather all the right-hand sides with the same
			// left-hand side in a set.
			def rules:Parser[Map[(F,String),Set[(VarTree[T],Seq[(String,Int)])]]]
			   	= rep(rule) ^^ (x => x groupBy (_._1) map ({ case
						(lhs, rhss) => (lhs,(rhss map (_._2)) toSet)}))

			// Putting it all together to parse a complete TD transducer
			def tdtrans = alpha~
						 beta~
						 states~
						 rules~
						 states ^^ {
						 case (sigma~delta~stateset~ruleset~initials) => new
							TDTreeTransducer[F,T](sigma,delta,stateset,ruleset,initials)
						 }

			// And setting it as the default parser
			def start = tdtrans
		}

}

/** A Top-Down Tree Transducer from F to T
 */
class TDTreeTransducer[F,T](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(F,String),Set[(VarTree[T],Seq[(String,Int)])]],
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

	/** Get the trees resulting from processing the input tree starting in
	 *  state q
	 */
	def applyState(q : String,tree : Tree[F]):Set[Tree[T]] ={
		val rhs = rules(tree.root,q)
		for(
			 (t,pairs) <- rhs; // Pairs of index/state to process and
			 				   // insert into the output tree t
			 (treeseq) <- Util.cartSet( // Take every valid combination of
				 			(pairs map  // subtrees
							 	{ case (st,ix) =>
					 				(st,tree.subtrees(ix)) 
					 			}) map (x => applyState(x._1,x._2))
							)
		   ) yield
			t.subAll(treeseq) // And build a new tree for each of them
	}

	/** Get the trees resulting from processing the input tree
	 */
	def apply(t:Tree[F]):Set[Tree[T]] = 
			(for(q <- q0) yield applyState(q,t)).flatten

	/** A tree has a defined output if it conforms to the input alphabet
	 */
	def isDefinedAt( t : Tree[F]):Boolean = sigma.verifyTree(t)
}



/** A Top-Down Weighted Tree Transducer from F to T
 */
class TDWTreeTransducer[F,T,R <: Semiring[R]](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(F,String),Set[(VarTree[T],Seq[(String,R,Int)])]],
	   val q0:Set[(String,R)]) extends AnyRef with WTreeTransducer[F,T,R] {
	
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

	/** Get the trees resulting from processing the input tree starting in
	 *  state q
	 */
	def applyState(q : String, w : R,tree : Tree[F]):Set[(Tree[T],R)] ={
		val rhs = rules(tree.root,q)
		for(
			 (t,triples) <- rhs; // Triples of index/coeff/state to process and
			 				   // insert into the output tree t
			 (treeseq) <- Util.cartSet( // Take every valid combination of
				 			(triples map  // subtrees
							 	{ case (st,c,ix) =>
					 				(st,c,tree.subtrees(ix)) 
					 			}) map (x => applyState(x._1,x._2*w,x._3))
							)
		   ) yield {
			val (trees,ws) = treeseq.unzip;
			(t.subAll(trees),ws.reduceLeft((x,y) => x + y)) // And build a new tree for each of them
		}
	}

	/** Get the trees resulting from processing the input tree
	 */
	def apply(t:Tree[F]):Set[(Tree[T],R)] = 
			(for((q,w) <- q0) yield applyState(q,w,t)).flatten

	/** A tree has a defined output if it conforms to the input alphabet
	 */
	def isDefinedAt( t : Tree[F]):Boolean = sigma.verifyTree(t)
}

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

/** Companion object of the Bottom-Up Tree Transducer class. Contains
 *  parser and factory methods.
 */ 
object BUTreeTransducer {
	/** Parser for BU Tree Transducers from F to T
	 */
	def BUTTParsers[F,T](implicit fps:ElementParsers[F],
			                      tps:ElementParsers[T]) = 
		new ElementParsers[BUTreeTransducer[F,T]] {
			// Parser for input symbols
			val tp:Parser[T] = tps
			// Parser for output symbols
			val fp:Parser[F] = fps
			// Parser for variable trees
			val vp:Parser[VarTree[T]] = VarTree.varTreeParser[T](tps)
			// Parser for the input alphabet
			def alpha:Parser[RankedAlphabet[F]] = 
				"{"~>RankedAlphabet.alphaParsers[F](fps)<~"}"
			// Parser for the output alphabet
			def beta:Parser[RankedAlphabet[T]] = 
				"{"~>RankedAlphabet.alphaParsers[T](tps)<~"}"
			// A state is a string
			def state:Parser[String] = nonTreeParser
			// Parser for a set of states
			def states:Parser[Set[String]] = 
					"{"~>repsep(state,",")<~"}" ^^ (_.toSet)
			// The left-hand side of a rule is an input symbol and a
			// sequence of states corresponding to the subtrees of that
			// input symbol
			def lhs:Parser[(F,Seq[String])] = 
					fp~opt("["~>repsep(state,",")<~"]") ^^ {
						case sym~Some(sts) => (sym,sts)
						case sym~None => (sym,Nil)
					}
			// The right-hand side of a rule is a state and a variable
			// tree, separated by a |
			def rhs = state~"|"~vp ^^ {case st~"|"~tr => (tr,st)}
			// A rule is a left-hand side and a right-hand side
			def rule = lhs~"|"~rhs ^^ {case lh~"|"~rh => (lh,rh)}
			// We need to group all the right-hand sides sharing the same
			// left-hand side into sets.
			def rules = rep(rule) ^^ (x => x groupBy (_._1) map ({ case
						(lhs, rhss) => (lhs,(rhss map (_._2)) toSet)})) 

			// Putting it all together to parse a complete bu transducer
			def butrans = alpha~
						 beta~
						 states~
						 rules~
						 states ^^ {
						 case (sigma~delta~stateset~ruleset~finals) => new
							BUTreeTransducer[F,T](sigma,delta,stateset,ruleset,finals)
						 }
			// Setting the defaul parser to be the complete transducer
			// parser
			def start = butrans


		}

}

/** A Bottom-Up Tree Transducer from F to T
 */ 
class BUTreeTransducer[F,T](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(F,Seq[String]),Set[(VarTree[T],String)]],
	   val fin:Set[String]) extends TreeTransducer[F,T] {

	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma       : "+sigma+
		"\nDelta       : "+delta+
		"\nStates      : "+states+
		"\nFinal states: "+fin+
		"\nRules       :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" | "+rhs+"\n")}
		ret.toString
	}

	/** Get the tree/state pairs resulting from the input tree.
	 */
	def applyState(t : Tree[F]):Set[(Tree[T],String)] = {
		val pairs = Util.cartSet(t.subtrees map applyState)
		(for((trees,states) <- pairs map (_.unzip) if rules.isDefinedAt(t.root,states) ;
			(vtree,state) <- rules(t.root,states)) yield 
				(vtree subAll trees,state)) toSet
	}

	/** Get the output trees determined by the input tree
	 */
	def apply( tree :Tree[F] ):Set[Tree[T]] = {
		(for((t,s) <- applyState(tree) if fin contains s) yield t) toSet
	}

	/** The transducer is defined for a tree conforming to the input
	 *  alphabet
	 */
	def isDefinedAt( t : Tree[F]):Boolean = sigma.verifyTree(t)

}



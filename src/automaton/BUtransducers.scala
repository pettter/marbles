package marbles.automaton
import marbles.util._

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


/** A Weighted Bottom-Up Tree Transducer from F to T
 */ 
class WBUTreeTransducer[F,T,R <: Semiring[R]](
	   val sigma:RankedAlphabet[F],
   	   val delta:RankedAlphabet[T],
	   val states:Set[String], //Should possibly be parameterised as well
	   val rules:Map[(F,Seq[String]),Set[(VarTree[T],String,Seq[R])]],
	   val fin:Set[(String,R)]) extends TreeTransducer[F,T] {

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
	def apply(tree:Tree[F]):Set[(Tree[T],R)] = { 
		(for((t,s) <- applyState(tree) if fin contains s) yield t) toSet
	}

	/** The transducer is defined for a tree conforming to the input
	 *  alphabet
	 */
	def isDefinedAt( t : Tree[F]):Boolean = sigma.verifyTree(t)

}

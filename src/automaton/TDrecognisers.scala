package marbles.automaton
import marbles.util._

/** Companion object of the class of Top-Down Nondeterministic Tree
 *  Automata. Contains parsers and (possibly) factory methods
 */
object TDNFTA {

	/** Parser for TDNFTAs. The format is
	 * {sym,...}   //Alphabet
	 * {state,...} //States
	 * sym|state | [state,...]
	 * ...
	 * {initialstate,...}
	 * In contrast to the BU version, there can be several lines with the
	 * same left-hand side, which is necessary for nondeterminism.
	 */
	def TDNFTAParsers[T](implicit tps:ElementParsers[T]) = 
		new ElementParsers[TDNFTA[T]] {
			// Conversion to the local Parser class of the implicit
			// parameter
			val tp:Parser[T] = tps
			// Parsing an alphabet
			def alpha:Parser[RankedAlphabet[T]] = 
				"{"~>RankedAlphabet.alphaParsers[T](tps)<~"}"
			// A single state is designated by a string
			def state:Parser[String] = nonTreeParser
			// A set of states
			def states:Parser[Set[String]] = 
					"{"~>repsep(state,",")<~"}" ^^ (_.toSet)
			// A sequence of states
			def stateseq:Parser[Seq[String]] = 
					"["~>repsep(state,",")<~"]"
			// A left-hand side, that is, a pair of symbol and state,
			// separated by a |
			def lhs:Parser[(T,String)] = tp~"|"~state ^^ 
				{case sym~"|"~st => (sym,st)}
			// A right-hand side is a state sequence (the states
			// propagating to the subtrees)
			def rhs = stateseq
			// A single rule is a left-hand side and a right-hand side
			def rule = lhs~"|"~rhs ^^ {case lh~"|"~rh => (lh,rh)}
			// This is where we transform the deterministic rules into
			// nondeterministic ones.
			def rules:Parser[Map[(T, String),Set[Seq[String]]]] = 
						rep(rule) ^^ ((x:Seq[((T,String),Seq[String])]) => 
								x.groupBy(_._1) map ({ 
									case (lhs, rhss) => (lhs,(rhss map (_._2)) toSet)
								})) 

			// Parser for the complete tdnfta
			def tdnfta = alpha~
						 states~
						 rules~
						 states ^^ {
						 case (sigma~stateset~ruleset~initials) => new
							TDNFTA[T](sigma,stateset,ruleset,initials)
						 }

			// Obviously, the starting parser is tdnfta
			def start = tdnfta


		}

}

/** A Top-Down Nondeterministic Finite Tree Automaton
 */
class TDNFTA[T](sigma:RankedAlphabet[T],
				states:Set[String],
				rules:Map[(T,String),Set[Seq[String]]],
				q0:Set[String]
		) extends NFTA[T] {

	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma         : "+sigma+
		"\nStates        : "+states+
		"\nRules         :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" | "+rhs+"\n")}
		ret.append("\nInitial states: "+q0)
		ret.toString
	}


	
	/** A TDNFTA is a function determining whether a tree is in the
	 *  language specified by the automaton or not
	 */
	def apply(tree:Tree[T]):Boolean = 
		(for(state <- q0) yield applyState(tree,state)).foldLeft(false)(_.||(_))
	

	
	/** A helper function, returning whether a tree is in the state
	 *  language of a specified state, i.e. if the automaton accepts the
	 *  tree, starting in the state
	 */
	def applyState(tree:Tree[T],state:String):Boolean =
		(for( stateseq <- rules(tree.root,state)) yield 
		 	(tree.subtrees zip stateseq).map({
				case (tree,state) => applyState(tree,state)
			}).foldLeft(true)(_.&&(_))
		).foldLeft(false)(_.||(_))
		
	/** Any tree that conforms to the alphabet can be considered by the
	 *  automaton.
	 */
	def isDefinedAt(tree:Tree[T]):Boolean = sigma.verifyTree(tree)
}


/** Companion object of the class of Top-Down Nondeterministic Weigted Tree
 *  Automata. Contains parsers and (possibly) factory methods
 */
object TDWFTA {

	/** Parser for TDWFTAs. The format is
	 * {sym,...}   //Alphabet
	 * {state,...} //States
	 * sym|state | [state|weight,...]
	 * ...
	 * {intialstate|weight,...}
	 * In contrast to the BU version, there can be several lines with the
	 * same left-hand side, which is necessary for nondeterminism.
	 */
	def TDWFTAParsers[T,R <: Semiring[R]](implicit tps:ElementParsers[T],
								                   rps:ElementParsers[R]) = 
		new ElementParsers[TDWFTA[T,R]] {
			// Conversion to the local Parser class of the implicit
			// parameter
			val tp:Parser[T] = tps
			val rp:Parser[R] = rps
			// Parsing an alphabet
			def alpha:Parser[RankedAlphabet[T]] = 
				"{"~>RankedAlphabet.alphaParsers[T](tps)<~"}"
			// A single state is designated by a string
			def state:Parser[String] = nonTreeParser
			// A set of states
			def states:Parser[Set[String]] = 
					"{"~>repsep(state,",")<~"}" ^^ (_.toSet)
			// A pair of state/weight
			def stweight:Parser[(String,R)] =
					state~"|"~rp ^^ {
						case st~"|"~w => (st,w)
					}
			// A sequence of states
			def stateseq:Parser[Seq[String]] = 
					"["~>repsep(state,",")<~"]"
			// with weights
			def stwseq:Parser[Seq[(String,R)]] =
					"["~>repsep(stweight,",")<~"]"
			// a set, not a sequence
			def stwset:Parser[Set[(String,R)]] =
					"{"~>repsep(stweight,",")<~"}" ^^ (_.toSet)
			// A left-hand side, that is, a pair of symbol and state,
			// separated by a |
			def lhs:Parser[(T,String)] = tp~"|"~state ^^ 
				{case sym~"|"~st => (sym,st)}
			// A right-hand side is a state sequence with weights (the states
			// propagating to the subtrees)
			def rhs = stwseq
			// A single rule is a left-hand side and a right-hand side
			def rule = lhs~"|"~rhs ^^ {case lh~"|"~rh => (lh,rh)}
			// This is where we transform the deterministic rules into
			// nondeterministic ones.
			def rules:Parser[Map[(T, String),Set[Seq[(String,R)]]]] = 
						rep(rule) ^^ ((x:Seq[((T,String),Seq[(String,R)])]) => 
								x.groupBy(_._1) map ({ 
									case (lhs, rhss) => (lhs,(rhss map (_._2)) toSet)
								})) 

			// Parser for the complete tdnfta
			def tdwfta = alpha~
						 states~
						 rules~
						 stwset ^^ {
						 case (sigma~stateset~ruleset~initials) => new
							TDWFTA[T,R](sigma,stateset,ruleset,initials)
						 }

			// Obviously, the starting parser is tdnfta
			def start = tdwfta


		}

}

/** A Top-Down Nondeterministic Weighted Finite Tree Automaton
 */
class TDWFTA[T,R <: Semiring[R]](sigma:RankedAlphabet[T],
				states:Set[String],
				rules:Map[(T,String),Set[Seq[(String,R)]]],
				q0:Set[(String,R)]
		) extends WFTA[T,R] {

	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma         : "+sigma+
		"\nStates        : "+states+
		"\nRules         :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" | "+rhs+"\n")}
		ret.append("\nInitial states: "+q0)
		ret.toString
	}

	private val rFactory = q0.head._2.factory
	
	/** A TDWFTA is a function determining the weight of a given tree
	 */
	def apply(tree:Tree[T]):R = 
		(for((state,weight) <- q0) yield weight*applyState(tree,state)).reduceLeft(_.+(_))
	

	
	/** A helper function, returning the weight given by a specific tree
	 *  from a given state
	 */
	def applyState(tree:Tree[T],state:String):R =
		tree.subtrees match {
			case Nil => rules((tree.root,state)).head.head._2
			case _ => (for( stateseq <- rules((tree.root,state))) yield 
		 	(tree.subtrees zip stateseq).map({
				case (tree,(st,w)) => w*applyState(tree,st)
			}).reduceLeft(_.+(_))
		).reduceLeft(_.+(_))
		}
		
	/** Any tree that conforms to the alphabet can be considered by the
	 *  automaton.
	 */
	def isDefinedAt(tree:Tree[T]):Boolean = sigma.verifyTree(tree)
}

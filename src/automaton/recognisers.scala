package marbles.automaton
import marbles.util._


trait TreeAutomaton[T,R] 
	extends PartialFunction[Tree[T],R]

abstract class NFTA[T] extends TreeAutomaton[T,Boolean]

/** Companion object of the class of Bottom-Up Nondeterministic Tree
 *  Automata. Contains parsers and (possibly) factory methods
 */
object BUNFTA {

	/** Parser for BUNFTAs. The format is
	 * {sym,...}   //Alphabet
	 * {state,...} //States
	 * sym[state,...] | {state,...}
	 * ...
	 * {finalstate,...}
	 */
	def BUNFTAParsers[T](implicit tps:ElementParsers[T]) = 
		new ElementParsers[BUNFTA[T]] {
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
			// The left-hand side of a rule
			def lhs:Parser[(T,Seq[String])] = 
					tp~opt("["~>repsep(state,",")<~"]") ^^ {
						case sym~Some(sts) => (sym,sts)
						case sym~None => (sym,Nil)
					}
			// The right-hand side of a rule
			def rhs = states
			// The combination, a rule
			def rule = lhs~"|"~rhs ^^ {case lh~"|"~rh => (lh,rh)}
			// Repeating a number of rules, and making a map out of them
			def rules = rep(rule) ^^ (_.toMap)

			// Complete parser for a BUNFTA
			def bunfta = alpha~
						 states~
						 rules~
						 states ^^ {
						 case (sigma~stateset~ruleset~finals) => new
							BUNFTA[T](sigma,stateset,ruleset,finals)
						 }
			def start = bunfta


		}

}

/** The class of Bottom-Up Nondeterministic Tree Automata
 */
class BUNFTA[T](val sigma:RankedAlphabet[T],
				val states:Set[String],
				val rules:Map[(T,Seq[String]),Set[String]],
				val fin:Set[String]
		)
	    extends NFTA[T]{

	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma       : "+sigma+
		"\nStates      : "+states+
		"\nRules       :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" | "+rhs+"\n")}
		ret.append("\nFinal states: "+fin)
		ret.toString
	}
	/** A BUNFTA is a function determining whether a tree is in the
	 *  language specified by the automaton or not
	 */
	def apply(tree:Tree[T]):Boolean = (fin & applyState(tree)).nonEmpty
	
	/** A helper function, returning what states the automaton is in after
	 *  processing the tree considered
	 */
	def applyState(tree:Tree[T]):Set[String] = {
		val stateseqs = Util.cartSet(tree.subtrees map applyState)
		(stateseqs flatMap (seq => rules.get((tree.root,seq)))) flatten 
	}

	/** The tree can be considered by the automaton if it conforms to the
	 *  alphabet.
	 */
	def isDefinedAt(tree:Tree[T]):Boolean = sigma.verifyTree(tree)
}

/** Companion object of the class of Top-Down Nondeterministic Tree
 *  Automata. Contains parsers and (possibly) factory methods
 */
object TDNFTA {

	/** Parser for TDNFTAs. The format is
	 * {sym,...}   //Alphabet
	 * {state,...} //States
	 * sym|state | [state,...]
	 * ...
	 * {finalstate,...}
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
								}) toMap) //TODO: groupBy fix can result in removing toMap

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


abstract class WFTA[T,R >: Semiring] extends TreeAutomaton[T,R] {
}



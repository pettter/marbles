package marbles.automaton
import marbles.util._

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
		) extends NFTA[T] {

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

object BUWFTA {
	/** Parser for BUNFTAs. The format is
	 * {sym,...}   //Alphabet
	 * {state,...} //States
	 * sym[state,...]|{state[weight,...],...} OR
	 * sym|{state[weight],...}
	 * ...
	 * {finalstate[factor],...}
	 */
	def BUWFTAParsers[T,R <: Semiring[R]](implicit tps:ElementParsers[T],
			                        rps:ElementParsers[R]) = 
		new ElementParsers[BUWFTA[T,R]] {
			// Conversion to the local Parser class of the implicit
			// parameters
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
			// A state with weights
			def stweight:Parser[(String,Seq[R])] = 
				state~"["~repsep(rp,",")~"]" ^^ {
					case st~"["~ws~"]" => (st,ws)
				}
			// A set of states with weights
			def stweights:Parser[Set[(String,Seq[R])]] = 
					"{"~>repsep(stweight,",")<~"}" ^^ (_.toSet)

			// The left-hand side of a rule
			def lhs:Parser[(T,Seq[String])] = 
					tp~opt("["~>repsep(state,",")<~"]") ^^ {
						case sym~Some(sts) => (sym,sts)
						case sym~None => (sym,Nil)
					}
			// The right-hand side of a rule
			def rhs = stweights
			// The combination, a rule
			def rule = lhs~"|"~rhs ^^ {case lh~"|"~rh => (lh,rh)}
			// Repeating a number of rules, and making a map out of them
			def rules = rep(rule) ^^ (_.groupBy(_._1._1) map {
				case (sym,tups) => (sym,(tups map {
						case ((sym,states),rhss) => (states,rhss)
					}) toMap) 
			})

			def finals = stweights ^^ (x => (x.map {
					case (st,ws) => (st,ws head)
				}).toMap)


			// Complete parser for a BUNFTA
			def buwfta = alpha~
						 states~
						 rules~
						 finals ^^ {
						 case (sigma~stateset~ruleset~finalsts) => new
							BUWFTA[T,R](sigma,stateset,ruleset,finalsts)
						 }
			
			def start = buwfta
		}
}

/** The class of Bottom-Up Nondeterministic Weighted Tree Automata
 */
class BUWFTA[T,R <% Semiring[R]](
				val sigma:RankedAlphabet[T],
				val states:Set[String],
				val rules:Map[T,Map[Seq[String],Set[(String,Seq[R])]]],
				//val rules:Map[(T,Seq[String]),Set[(String,R)]],
				// Rules could be represented as vectors.
				val fin:Map[String,R]
		) extends WFTA[T,R]{

	private val rFactory = fin.values.head.factory
	
	override def toString:String = {
		var ret:StringBuilder = new StringBuilder( 
		  "Sigma       : "+sigma+
		"\nStates      : "+states+
		"\nRules       :\n")
		rules foreach {case (lhs,rhs) => ret.append("    "+lhs+" | "+rhs+"\n")}
		ret.append("\nFinal states: "+fin)
		ret.toString
	}
	/** A BUWFTA is a function determining the weight of a specific tree.
	 */
	def apply(tree:Tree[T]):R = {
		val swmap = applyState(tree)
		(for(sym <- fin.keys) yield {
				fin(sym) * swmap.getOrElse(sym,rFactory.zero)
			}) reduceLeft ((x,y) => x + y)
	}

	// Get the "matrix multiplication" of the incoming right-hand side
	// 'matrix' and weight 'vector'
	private def getPairs(rhss:Set[(String,Seq[R])],
				         weights:Seq[R]
				):Seq[(String,R)] = for((state,coeffs) <- rhss toList) yield 
				  weights match {
					case Nil => (state,coeffs head)
					case _ => (state,((weights zip coeffs).map(
							(t)   => t._1 * t._2
						)).reduceLeft(
							(x,y) => x + y
						))
				  }

	/** A helper function, returning the state-to-weight map the automaton is in after
	 *  processing the tree considered
	 */
	def applyState(tree:Tree[T]):Map[String,R] = {
		// This is the subgroup of the rules that we need to consider on
		// this tree
		val rulemap:Map[Seq[String],Set[(String,Seq[R])]] = rules(tree.root);
		// Get the possible combinations of state/weight sequences from
		// nondeterministically processing the subtrees
		val stateseqs:Set[Seq[(String,R)]] = Util.cartSet((tree.subtrees map applyState) map (_.toSet))
		// Loop over the state/weight sequences and get all the resulting
		// weights from the getPairs helper function. Finish by flattening
		// it all into a single list instead of a list of lists
		val respairs:Seq[(String,R)] = 
			(for(pairs <- stateseqs toList) yield {
				val (states,weights) = pairs unzip;
				rulemap.get(states) match {
					case None => Nil
					case Some(rhss) => getPairs(rhss,weights)
				}
			}).flatten
		// This expression looks more complex than it is. It transforms a
		// list of tuples into a map where every left-hand side is unique
		// and every right-hand side is the sum of the right-hand sides
		// associated with that left-hand side in the original list.
		(respairs.groupBy(_._1) map { case (lhs,rhss) =>
   			(lhs,(rhss map (_._2)) reduceLeft ((x,y) => x + y))}).toMap
	}

	/** The tree can be considered by the automaton if it conforms to the
	 *  alphabet.
	 */
	def isDefinedAt(tree:Tree[T]):Boolean = sigma.verifyTree(tree)
}




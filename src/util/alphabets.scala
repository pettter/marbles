package marbles
package util
import scala.collection.immutable._

//object Alphabet {
//	implicit def alphaParsers[T](implicit tp:ElementParsers[T]):
//		ElementParsers[Alphabet[T]] = new ElementParsers[Alphabet[T]] {
//			def start = repsep(tp,",") ^^ (new Alphabet(_))
//		}
//
//}

/** A set of valid symbols for use in a tree
 */
class Alphabet[T] extends scala.collection.immutable.HashSet[T] {
	/** Verify that the tree conforms to the alphabet, i.e. only has
	 *  symbols which are part of the alphabet.
	 */
	def verifyTree(t: Tree[T]):Boolean = 
		this.contains(t.root) && 
		t.subtrees.forall(verifyTree _)
}

/** Companion object of the RankedAlphabet class. Contains factory
 *  functions and parsers
 */
object RankedAlphabet {

	/** Parse a ranked alphabet on the form {symbol|rank,...}
	 */
	implicit def alphaParsers[T](implicit tp:ElementParsers[T])= 
		new ElementParsers[RankedAlphabet[T]] {
			implicit def foo = ElementParsers.intParsers // Why is this necessary?
			def tups:Parser[(T,Int)] = ElementParsers.withWeight[T,Int]
			def start = repsep(tups,",") ^^ (x =>
					RankedAlphabet(x toMap))
		}

	/** Standard factory method
	 */
	def apply[T](map:scala.collection.Map[T,Int]) = new RankedAlphabet(map)
}

/** A ranked alphabet, a set of valid ranked symbols for use in a tree
 */
class RankedAlphabet[T] (val map:scala.collection.Map[T,Int]) {

	override def toString:String = {
	  val ret:StringBuilder = new StringBuilder("{")
	  map foreach {case (s,r) => ret.append(s+"|"+r+",") }
	  ret.stripSuffix(",") + "}"
	}



	/** Get the rank of a symbol
	 */
	def apply(x:T) = map.apply(x)

	/** Verify that a tree conforms to the alphabet
	 */
	def verifyTree(t: Tree[T]):Boolean = 
		map.isDefinedAt(t.root) && 
		t.subtrees.size == map(t.root) &&
		t.subtrees.forall(verifyTree _)

	/** Get the symbols of a certain rank
	 */
	def symsRanked(r:Int) = map.filter(_._2 == r) keys

	/** Get leaf symbols
	 */
	def leaves = symsRanked(0)

	/** Construct a new RankedAlphabet with every rank multiplied by n
	 */
	def multiRanked(n:Int) = new RankedAlphabet(map mapValues(_ * n))

}

/** Companion object of the Variable class, contains factory and extractor
 *  methods
 */
object Variable{
	/** Construct a variable from a string (i.e. parsing it)
	 */
	def apply(s:String) = varParser.parseObj(s).get
//		if ((s startsWith "{") &&
//			(s endsWith "}"))
//			  new Variable(s.substring(1,s.length-1).toInt)
//		else
//			throw new java.lang.NumberFormatException()

	/** Construct a new Variable with the given number
	 */
	def apply(v:Int) = new Variable(v)

	/** Extracting the embedded number
	 */
	def unapply(v:Variable) = Some(v.v)
	
	/** Implicit parser for parsing variables on the form {number}
	 */
	implicit def varParser:ElementParsers[Variable] = 
		new ElementParsers[Variable] {
			def start = "{"~>intParser<~"}" ^^ (Variable(_))
		}

}

/** The Variable class represents variables in, e.g. transducer rules.
 */
class Variable(val v:Int){
	override def toString:String = "{"+v+"}"
	override def hashCode:Int = 41 + v*41
	override def equals(other:Any) = other match {
		case that:Variable => this.v == that.v
		case _ => false
	}
}


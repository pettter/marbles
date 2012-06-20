package marbles.util
import scala.util.parsing.combinator.JavaTokenParsers

/** This singleton holds various utility function useful throughout the
 *  system.
  */
object Util {

	/** Construct the 'cartesian set' of all possible combinations of
	 *  choices from a sequence of sets.
	 */
	def cartSet[T](sets: Seq[Set[T]]): Set[Seq[T]] = {
		sets.toList match {
			case Nil => Set(Seq())
			case x :: xs => 
				val l = cartSet(xs)
				x.flatMap(e => l.map(f => e +: f))
		}
	}

	def getUniqueState(states:Set[String],
	                   suggestion:String="",
			   iter:Int=0
			  ):String = {
	   val s = if(iter == 0)
	       suggestion
             else
	       suggestion + iter
	   if(!states.contains(s))
	       s
	   else
	       getUniqueState(states,suggestion,iter+1)
	}

//	def singleton[T](implicit man: reflect.Manifest[T]) = {
//		val name = man.erasure.getName() 
//		assert(name endsWith "$", "Not an object: " + name)
//		val clazz = java.lang.Class.forName(name)
//		clazz.getField("MODULE$").get(clazz).asInstanceOf[T]
//	}  

}

/** Companion object of the ElementParsers class. Contains various
 *  implicits for easy access
 */
object ElementParsers extends JavaTokenParsers {

	/** A standard string is any sequence of characters not 'reserved' for
	 *  use in trees and/or sets/variables
	 */
	implicit def nonTreeParsers:ElementParsers[String] = new ElementParsers[String] { 
		def start = nonTreeParser
	}

	/** Standard parser for floating point numbers
	 */
	implicit def numParsers:ElementParsers[Double] = new ElementParsers[Double] {
		def start:Parser[Double] = numParser
	}
	
	/** Standard parser for integers
	 */
	implicit def intParsers:ElementParsers[Int] = new ElementParsers[Int] {
		def start =	intParser
	}

	/** A simple way to construct a parser for Either combinations
	 */
	def either[U,V](implicit u:ElementParsers[U],
							 v:ElementParsers[V]):
			ElementParsers[Either[U,V]] = new ElementParsers[Either[U,V]] { 
		def start = (u ^^ (Left(_))) | (v ^^ (Right(_)))
	}
	
	/** Anything with a weight, separated by a | character
	 */
	def withWeight[U,V](implicit us:ElementParsers[U],
									  vs:ElementParsers[V]) = 
		new ElementParsers[(U,V)] { 
			def u:Parser[U] = us
			def v:Parser[V] = vs
			def tuple:Parser[(U,V)] = {
				(u~"|"~v) ^^ { case s~"|"~w => (s,w)}
			}

			def start = tuple
		}

}

/** Class ElementParsers is an extension of JavaTokenParsers intended for
 *  use as a more general combinator parser, i.e. any ElementParsers can
 *  use any other ElementParsers as a component through implicit conversion
 *  to a this.Parser[T] for the particular type covered.
 */
abstract class ElementParsers[T] extends JavaTokenParsers {
	import scala.util.parsing.input._
	import scala.collection.immutable.PagedSeq
	
	val nonTreeParser:Parser[String] = "[^\\{\\}\\[\\]\\|,]*".r
	
	val numParser = floatingPointNumber ^^ (_.toDouble)
	
	val intParser:Parser[Int] = wholeNumber ^^ (_.toInt)

	def either[U,V](u:Parser[U],
					v:Parser[V]) = (u ^^ (Left(_))) | (v ^^ (Right(_)))

	/** Implicit conversion from any ElementParsers[T] to the local
	 *  equivalent Parser[T]
	 */
	implicit def elementParsersToParser[U](ep:ElementParsers[U]):Parser[U] = 
		ep.start.asInstanceOf[Parser[U]]
	
	/** The parser for the eventual resulting object. Must be defined in
	 *  any concrete subclass
	 */
	def start:Parser[T]

	/** Parse an object using the start parser, return None if the parse
	 *  fails Some(result) if it succeeds
	 */
	def parseObj(in:Input):Option[T] = {
		val res = parseAll(start,in)
		if (res.successful)
			Some(res.get)
		else {
			println(res)
			None
		}
	}

	/** Parse an object using the start parser, return None if the parse
	 *  fails Some(result) if it succeeds
	 */
	def parseObj(in:java.lang.CharSequence):Option[T] = 
		parseObj(new CharSequenceReader(in))

	/** Parse an object using the start parser, return None if the parse
	 *  fails Some(result) if it succeeds
	 */
	def parseObj(in:java.io.Reader):Option[T] =
		parseObj(new PagedSeqReader(PagedSeq.fromReader(in)))
}

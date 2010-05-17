package marbles
package util

class Alphabet[T] extends scala.collection.immutable.HashSet[T] {
	def verifyTree(t: Tree[T]):Boolean = 
		this.contains(t.root) && 
		t.subtrees.forall(verifyTree _)
}



class RankedAlphabet[T] (val map:scala.collection.Map[T,Int]) {

	override def toString:String = map.toString

	def apply(x:T) = map.apply(x)

	def verifyTree(t: Tree[T]):Boolean = 
		map.isDefinedAt(t.root) && 
		t.subtrees.size == map(t.root) &&
		t.subtrees.forall(verifyTree _)

	def symsRanked(r:Int) = map.filter(_._2 == r) keys

	def leaves = symsRanked(0)

	def multiRanked(n:Int) = new RankedAlphabet(map mapValues(_ * n))

}

object Variable{
	def apply(s:String) = 
		if ((s startsWith "{") &&
			(s endsWith "}"))
			  new Variable(s.substring(1,s.length-1).toInt)
		else
			throw new java.lang.NumberFormatException()

	def apply(v:Int) = new Variable(v)

	def unapply(v:Variable) = Some(v.v)
}


class Variable(val v:Int){
	override def toString:String = "{"+v+"}"
	override def hashCode:Int = 41 + v*41
	override def equals(other:Any) = other match {
		case that:Variable => this.v == that.v
		case _ => false
	}
	

}


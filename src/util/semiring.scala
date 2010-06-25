package marbles
package util

trait SemiringFactory[T] {
	def _0:T
	def zero = _0
	def _1:T
	def one = _1
}


trait Semiring[T]{
	def factory:SemiringFactory[T]

	def +(that:T):T
	def *(that:T):T
}

object Reals extends SemiringFactory[Reals] {
	def _0 = new Reals(0.0)
	def _1 = new Reals(1.0)
	def apply(d:Double) = new Reals(d)
	implicit def realsParsers:ElementParsers[Reals] = new ElementParsers[Reals] {
		def num:Parser[Reals] = numParser ^^ Reals.apply
		def start = num
	}

}

class Reals(val d:Double) extends Semiring[Reals] {
	override def toString = d.toString
	def factory = Reals
	def +(that:Reals) = Reals(this.d + that.d)
	def *(that:Reals) = Reals(this.d * that.d)
}

object MaxPlus extends SemiringFactory[MaxPlus] {
	def _0 = new MaxPlus(java.lang.Double.NEGATIVE_INFINITY)
	def _1 = new MaxPlus(0.0)
	def apply(d:Double) = new MaxPlus(d)
	implicit def maxPlusParsers:ElementParsers[MaxPlus] = new ElementParsers[MaxPlus] {
		def num:Parser[MaxPlus] = numParser ^^ MaxPlus.apply
		def start = num
	}
}

class MaxPlus(val d:Double) extends Semiring[MaxPlus] {
	override def toString = d.toString
	def factory = MaxPlus
	def +(that:MaxPlus) = MaxPlus(this.d.max(that.d))
	def *(that:MaxPlus) = MaxPlus(this.d + that.d)
}

object BoolRing extends SemiringFactory[BoolRing] {
	def _0 = new BoolRing(false)
	def _1 = new BoolRing(true)
	def apply(b:Boolean):BoolRing = new BoolRing(b)
	implicit def boolRingParsers:ElementParsers[BoolRing] = 
			new ElementParsers[BoolRing] {
				def bool:Parser[BoolRing] = "true"  ^^ (x => BoolRing(true )) |
											"false" ^^ (x => BoolRing(false))
				def start = bool
			}
}

class BoolRing(val b:Boolean) extends Semiring[BoolRing] {
	override def toString = b.toString
	def factory = BoolRing
	def +(that:BoolRing) = BoolRing(this.b || that.b)
	def *(that:BoolRing) = BoolRing(this.b && that.b)
}



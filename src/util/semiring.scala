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
}

class Reals(val d:Double) extends Semiring[Reals] {
	def factory = Reals
	def +(that:Reals) = Reals(this.d + that.d)
	def *(that:Reals) = Reals(this.d * that.d)
}

object MaxPlus extends SemiringFactory[MaxPlus] {
	def _0 = new MaxPlus(java.lang.Double.NEGATIVE_INFINITY)
	def _1 = new MaxPlus(0.0)
	def apply(d:Double) = new MaxPlus(d)
}

class MaxPlus(val d:Double) extends Semiring[MaxPlus] {
	def factory = MaxPlus
	def +(that:MaxPlus) = MaxPlus(this.d.max(that.d))
	def *(that:MaxPlus) = MaxPlus(this.d + that.d)
}

object BoolRing extends SemiringFactory[BoolRing] {
	def _0 = new BoolRing(false)
	def _1 = new BoolRing(true)
	def apply(b:Boolean):BoolRing = new BoolRing(b)
}

class BoolRing(val b:Boolean) extends Semiring[BoolRing] {
	def factory = BoolRing
	def +(that:BoolRing) = BoolRing(this.b || that.b)
	def *(that:BoolRing) = BoolRing(this.b && that.b)
}



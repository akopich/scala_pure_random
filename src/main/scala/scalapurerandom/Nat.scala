package scalapurerandom

import cats.data.NonEmptyVector
import spire.algebra._

import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

sealed trait Nat {
  def value: Int

  override def toString: String = value.toString
}

case object Zero extends Nat {
  override def value: Int = 0
}

sealed trait Pos extends Nat

case object One extends Pos {
  override def value: Int = 1
}

case class More(value: Int) extends Pos

class PosHelper(val sc: StringContext) extends AnyVal {
  def p(): Pos = mkPos(sc.parts.mkString.toInt)
}

class NatHelper(val sc: StringContext) extends AnyVal {
  def n(): Nat = mkNat(sc.parts.mkString.toInt)
}

trait NatHelperTrait { self =>
  implicit def toInt(a: Nat): Int = a.value

  implicit def wrapContextPos(sc: StringContext): PosHelper = new PosHelper(sc)

  implicit def wrapContextNat(sc: StringContext): NatHelper = new NatHelper(sc)

  def mkPos(value: Int): Pos = {
    require(value > 0, "Only for positive numbers")
    value match {
      case 1 => One
      case _ => More(value)
    }
  }

  def mkNat(value: Int): Nat = {
    require(value >= 0, "Only for natural numbers")
    value match {
      case 0 => Zero
      case _ => mkPos(value)
    }
  }

  def dec(a: More): Pos = a match {
    case More(2)     => One
    case More(value) => More(value - 1)
  }

  def dec(a: Pos): Nat = a match {
    case One     => Zero
    case m: More => dec(m)
  }

  def inc(a: Nat): Pos = a match {
    case Zero    => One
    case aa: Pos => inc(aa)
  }

  def inc(a: Pos): Pos = a match {
    case One         => More(2)
    case More(value) => More(value + 1)
  }

  def sum(a: Nat, b: Nat): Nat = (a, b) match {
    case (Zero, _)          => b
    case (_, Zero)          => a
    case (ap: Pos, bp: Pos) => sum(ap, bp)
  }

  def sum(a: Nat, b: Pos): Pos = (a, b) match {
    case (Zero, _)    => b
    case (ap: Pos, _) => sum(ap, b)
  }

  def sum(a: Pos, b: Pos): Pos = (a, b) match {
    case (One, _)             => inc(b)
    case (_, One)             => inc(a)
    case (More(va), More(vb)) => More(va + vb)
  }

  def prod(a: Pos, b: Pos): Pos = (a, b) match {
    case (One, _)             => b
    case (_, One)             => a
    case (More(aa), More(bb)) => More(aa * bb)
  }

  def prod(a: Nat, b: Nat): Nat = (a, b) match {
    case (Zero, _) |
         (_, Zero)          => Zero
    case (ap: Pos, bp: Pos) => prod(ap, bp)
  }

  implicit val natIsRig: Rig[Nat] = new Rig[Nat] {
    override def one: Nat = One

    override def zero: Nat = Zero

    override def plus(x: Nat, y: Nat): Nat = self.sum(x, y)

    override def times(x: Nat, y: Nat): Nat = prod(x, y)
  }

  implicit class NatTimesWrap(n: Nat) {
    def times[T](f: => T): Vector[T] = Vector.fill(n)(f)

    def parTimes[T: ClassTag](f: => T): Vector[T] = (n times None).par map (_ => f) seq
  }

  def pow(base: Pos)(power: Pos): Pos = power match {
    case One     => base
    case m: More => prod(base, pow(base)(dec(m)))
  }

  implicit class PosTimesWrap(p: Pos) {
    def times[T](f: => T): NEV[T] = NonEmptyVector(f, dec(p) times f)

    def parTimes[T: ClassTag](f: => T): NonEmptyVector[T] = {
      val head +: tail = Vector.fill(p)(None).par.map(_ => f).seq
      NonEmptyVector(head, tail)
    }
  }

}

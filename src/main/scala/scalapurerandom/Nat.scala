package scalapurerandom

import algebra.ring.{AdditiveMonoid, AdditiveSemigroup, MultiplicativeMonoid, MultiplicativeSemigroup, Rng}
import cats.Applicative
import cats.data.{NonEmptyList, NonEmptyVector}
import cats.kernel.Eq
import spire.syntax.field._
import cats.implicits._

import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

case class Nat(toInt: Int) {
  override def toString: String = s"Nat($toInt)"
}

class PosInt(val dec: Nat) {
  def toInt = dec.toInt + 1

  override def toString: String = s"Pos(${dec.toInt + 1})"
}

object Nat {
  def apply(int: Int): Nat = {
    require(int >= 0, "Negative isn't natural")
    new Nat(int)
  }
}

object PosInt {
  def apply(int: Int): PosInt = {
    require(int > 0, "The value isn't positive")
    new PosInt(Nat(int - 1))
  }
}

class PosHelper(val sc: StringContext) extends AnyVal {
  def p(): PosInt = PosInt(sc.parts.mkString.toInt)
}

class NatHelper(val sc: StringContext) extends AnyVal {
  def n(): Nat = Nat(sc.parts.mkString.toInt)
}

trait NatHelperTrait { self =>
  def replicateA[F[_], A](n: PosInt, fa: F[A])(implicit F: Applicative[F]): F[NEL[A]] = {
    val tail = F.replicateA(n.dec.toInt, fa)
    F.map2(fa, tail)(NonEmptyList.apply)
  }

  implicit def wrapContextPos(sc: StringContext): PosHelper = new PosHelper(sc)

  implicit def wrapContextNat(sc: StringContext): NatHelper = new NatHelper(sc)

  def inc(n : Nat): PosInt = new PosInt(n)

  def dec(p: PosInt): Nat = p.dec

  implicit class NatTimesWrap(n: Nat) {
    def times[T](f: => T): Vector[T] = Vector.fill(n.toInt)(f)

    def parTimes[T: ClassTag](f: => T): Vector[T] = (n times None).par map (_ => f) seq
  }

  implicit class PosTimesWrap(p: PosInt) {
    def times[T](f: => T): NEV[T] = NonEmptyVector(f, dec(p) times f)

    def parTimes[T: ClassTag](f: => T): NonEmptyVector[T] = {
      val head +: tail = Vector.fill(p.toInt)(None).par.map(_ => f).seq
      NonEmptyVector(head, tail)
    }
  }

  implicit lazy val natIsAdditiveMonoid: AdditiveMonoid[Nat] = new AdditiveMonoid[Nat] {
    override def zero: Nat = n"0"

    override def plus(x: Nat, y: Nat): Nat = new Nat(x.toInt + y.toInt)
  }

  implicit lazy val natIsMultMonoid: MultiplicativeSemigroup[Nat] = (x: Nat, y: Nat) => Nat(x.toInt * y.toInt)

  implicit lazy val posIsAdditiveSemigroup: AdditiveSemigroup[PosInt] = (x: PosInt, y: PosInt) => inc(x.dec + y.dec)

  implicit lazy val posIsMultSemigroup: MultiplicativeMonoid[PosInt] = new MultiplicativeMonoid[PosInt] {
    override def one: PosInt = p"1"

    override def times(x: PosInt, y: PosInt): PosInt = inc(x.dec * y.dec + x.dec + y.dec)
  }

  implicit lazy val eqNat = new Eq[Nat] {
    override def eqv(x: Nat, y: Nat): Boolean = x.toInt == y.toInt
  }

  implicit lazy val eqPos = new Eq[PosInt] {
    override def eqv(x: PosInt, y: PosInt): Boolean = x.dec === y.dec
  }

  def pow(base: PosInt, nat: Nat): PosInt = (nat times base).foldLeft(p"1")(posIsMultSemigroup.times)

  def pow(base: PosInt, pos: PosInt): PosInt = base * pow(base, pos.dec)
}

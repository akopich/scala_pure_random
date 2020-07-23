package scalapurerandom

import cats.Applicative
import cats.data.NonEmptyVector

import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

case class Nat(toInt: Int) {
  require(toInt >= 0, "Negative isn't natural")

  override def toString: String = s"Nat($toInt)"
}

case class Pos(dec : Nat) {
  def toInt = dec.toInt + 1

  override def toString: String = s"Pos(${dec.toInt + 1})"
}

object Pos {
  def apply(int: Int): Pos = {
    require(int > 0, "The value isn't positive")
    Pos(Nat(int - 1))
  }
}

class PosHelper(val sc: StringContext) extends AnyVal {
  def p(): Pos = Pos(sc.parts.mkString.toInt)
}

class NatHelper(val sc: StringContext) extends AnyVal {
  def n(): Nat = Nat(sc.parts.mkString.toInt)
}

trait NatHelperTrait { self =>
  def replicateA[F[_], A](n: Pos, fa: F[A])(implicit F: Applicative[F]): F[NEV[A]] = {
    val tail = F.map(F.replicateA(n.dec.toInt, fa))(_.toVector)
    F.map2(fa, tail)(NonEmptyVector.apply)
  }

  implicit def toInt(a: Nat): Int = a.toInt

  implicit def wrapContextPos(sc: StringContext): PosHelper = new PosHelper(sc)

  implicit def wrapContextNat(sc: StringContext): NatHelper = new NatHelper(sc)

  def inc(n : Nat): Pos = Pos(n)

  def dec(p: Pos): Nat = p.dec

  implicit class NatTimesWrap(n: Nat) {
    def times[T](f: => T): Vector[T] = Vector.fill(n)(f)

    def parTimes[T: ClassTag](f: => T): Vector[T] = (n times None).par map (_ => f) seq
  }

  implicit class PosTimesWrap(p: Pos) {
    def times[T](f: => T): NEV[T] = NonEmptyVector(f, dec(p) times f)

    def parTimes[T: ClassTag](f: => T): NonEmptyVector[T] = {
      val head +: tail = Vector.fill(p.toInt)(None).par.map(_ => f).seq
      NonEmptyVector(head, tail)
    }
  }

}

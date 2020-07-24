package scalapurerandom

import cats.Applicative
import cats.data.NonEmptyVector

import scala.collection.parallel.CollectionConverters._
import scala.reflect.ClassTag

class Nat(val toInt: Int) extends AnyVal {

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
  def replicateA[F[_], A](n: PosInt, fa: F[A])(implicit F: Applicative[F]): F[NEV[A]] = {
    val tail = F.map(F.replicateA(n.dec.toInt, fa))(_.toVector)
    F.map2(fa, tail)(NonEmptyVector.apply)
  }

  implicit def toInt(a: Nat): Int = a.toInt

  implicit def wrapContextPos(sc: StringContext): PosHelper = new PosHelper(sc)

  implicit def wrapContextNat(sc: StringContext): NatHelper = new NatHelper(sc)

  def inc(n : Nat): PosInt = PosInt(n)

  def dec(p: PosInt): Nat = p.dec

  implicit class NatTimesWrap(n: Nat) {
    def times[T](f: => T): Vector[T] = Vector.fill(n)(f)

    def parTimes[T: ClassTag](f: => T): Vector[T] = (n times None).par map (_ => f) seq
  }

  implicit class PosTimesWrap(p: PosInt) {
    def times[T](f: => T): NEV[T] = NonEmptyVector(f, dec(p) times f)

    def parTimes[T: ClassTag](f: => T): NonEmptyVector[T] = {
      val head +: tail = Vector.fill(p.toInt)(None).par.map(_ => f).seq
      NonEmptyVector(head, tail)
    }
  }

}

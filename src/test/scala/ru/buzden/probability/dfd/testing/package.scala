package ru.buzden.probability.dfd

import cats.data.Validated
import cats.syntax.apply._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.scalacheck.Gen._
import org.scalacheck.cats.implicits._
import org.specs2.matcher.describe.Diffable
import ru.buzden.util.numeric.syntax.{one, zero}
import spire.math.{Rational, SafeLong}

package object testing {
  // --- Gen-related utility functions ---

  def nonNegNum[N: Numeric:Choose]: Gen[N] = frequency(1 -> zero[N], 99 -> posNum[N])

  def rational(numerator: Gen[Long]): Gen[Rational] = (numerator, posNum[Long]).mapN(Rational.apply)

  val posRational: Gen[Rational] = rational(posNum[Long])
  val nonNegRational: Gen[Rational] = rational(nonNegNum[Long])
  def between0and1[N: Numeric:Choose]: Gen[N] = chooseNum(zero[N], one[N])

  def listOfNWithNonZero[A: Numeric](n: Int, genA: Gen[A]): Gen[List[A]] =
    listOfN(n, genA) `suchThat` { _.exists(_ =!= zero[A]) }

  def nonEmptyListOfDistinct[A](genA: Gen[A]): Gen[List[A]] =
  // todo to use analogue of `.distinct` based on `cats.Eq`.
    nonEmptyListOf(genA) `map` (_.distinct)

  // --- Other utility functions ---

  def normalize(l: List[Rational]): List[Rational] = {
    val sum = l.sum
    l `map` (_ / sum)
  }

  // --- Instances for testing ---

  implicit val rationalIsProbability: Probability[Rational] = new Fractional[Rational] {
    override def plus(x: Rational, y: Rational): Rational = x + y
    override def minus(x: Rational, y: Rational): Rational = x - y

    override def times(x: Rational, y: Rational): Rational = x * y
    override def div(x: Rational, y: Rational): Rational = x / y

    override def negate(x: Rational): Rational = -x

    override def compare(x: Rational, y: Rational): Int = x `compare` y

    override def fromInt(x: Int): Rational = Rational(x)

    override def toInt(x: Rational): Int = x.toInt
    override def toLong(x: Rational): Long = x.toLong
    override def toFloat(x: Rational): Float = x.toFloat
    override def toDouble(x: Rational): Double = x.toDouble
  }

  implicit val safeLongIsIntegral: Integral[SafeLong] = new Integral[SafeLong] {
    override def plus(x: SafeLong, y: SafeLong): SafeLong = x + y
    override def minus(x: SafeLong, y: SafeLong): SafeLong = x - y

    override def times(x: SafeLong, y: SafeLong): SafeLong = x * y

    override def quot(x: SafeLong, y: SafeLong): SafeLong = x / y
    override def rem(x: SafeLong, y: SafeLong): SafeLong = x % y

    override def negate(x: SafeLong): SafeLong = -x

    override def compare(x: SafeLong, y: SafeLong): Int = x `compare` y

    override def fromInt(x: Int): SafeLong = SafeLong(x)

    override def toInt(x: SafeLong): Int = x.toInt
    override def toLong(x: SafeLong): Long = x.toLong
    override def toFloat(x: SafeLong): Float = x.toFloat
    override def toDouble(x: SafeLong): Double = x.toDouble
  }

  implicit val arbSafeLong: Arbitrary[SafeLong] = Arbitrary(Gen.oneOf(
    arbitrary[Long].map(SafeLong(_)),
    arbitrary[BigInt].map(SafeLong(_)),
  ))
  implicit val cogenSafeLong: Cogen[SafeLong] = Cogen.cogenLong.contramap(_.toLong)
  implicit val cogenRational: Cogen[Rational] = cogenSafeLong.contramap { r => r.numerator + r.denominator }

  implicit def cogen4dfd[A: Cogen:Ordering, P: Cogen]: Cogen[DiscreteFiniteDistribution[A, P]] =
    Cogen.cogenVector[(A, P)].contramap { dfd =>
      dfd.support.toVector.sorted.map(a => (a, dfd.pmf(a)))
    }

  implicit val chooseBigInt: Choose[BigInt] = (min, max) => {
    if (min > max) throw new Choose.IllegalBoundsError(min, max) // they originally throw :-(
    val len = max - min

    Gen.listOfN((len.bitLength + 7) / 8, arbitrary[Byte])
      .map { bs => BigInt(1, Array(bs:_*)) }
      .suchThat { x => BigInt(0) <= x && x <= len }
      .map (_ + min)
  }

  implicit val chooseSafeLong: Choose[SafeLong] = Choose.xmap(SafeLong(_:BigInt), _.toBigInt)

  // a_min * b_min <= x / y <= a_max / b_max
  // assuming denominators to be positive,
  // a_min * b_max * y <= b_min * b_max * x <= a_max * b_min * y
  //
  // This implementation assumes that Rational's denominators are positive
  implicit val chooseRational: Choose[Rational] = (min, max) => for {
    y <- Gen.posNum[SafeLong]
    u <- Gen.chooseNum(min.numerator * max.denominator * y, max.numerator * min.denominator * y)
  } yield Rational(u, y * min.denominator * max.denominator)

  // todo to do this with contramap when it's possible
  implicit def dfdDiffable[A, P]: Diffable[DiscreteFiniteDistribution[A, P]] = { (actual, expected) =>
    def dfd2map(dfd: DiscreteFiniteDistribution[A, P]): Map[A, P] =
      Map(dfd.support.toList.map(a => (a, dfd.pmf(a))):_*)
    val diffm = implicitly[Diffable[Map[A, P]]]
    diffm.diff(dfd2map(actual), dfd2map(expected))
  }

  implicit def validatedDiffable[A: Diffable, E: Diffable]: Diffable[Validated[E, A]] = { (actual, expected) =>
    // todo to reimplement this with nice rendering
    val diffe = implicitly[Diffable[Either[E, A]]]
    diffe.diff(actual.toEither, expected.toEither)
  }
}
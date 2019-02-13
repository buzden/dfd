package ru.buzden

import cats.data.Validated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.{Cogen, Gen}
import org.scalacheck.Gen._
import org.specs2.matcher.describe.Diffable
import ru.buzden.util.numeric.syntax.zero
import spire.math.{Rational, SafeLong}

package object testing {
  // --- Utility functions ---

  def nonNegNum[N: Numeric:Choose]: Gen[N] = frequency(1 -> zero[N], 99 -> posNum[N])

  // --- Gen instances ---

  def nonEmptyListOfDistinct[A](genA: Gen[A]): Gen[List[A]] =
    // todo to use analogue of `.distinct` based on `cats.Eq`.
    nonEmptyListOf(genA) `map` (_.distinct) `map` (_ `takeRight` 15)

  // --- Choose instances ---

  implicit val chooseBigInt: Choose[BigInt] = (min, max) => {
    if (min > max) throw new Choose.IllegalBoundsError(min, max) // they originally throw :-(
    val len = max - min

    Gen.listOfN((len.bitLength + 7) / 8, arbitrary[Byte])
      .map { bs => BigInt(1, Array(bs:_*)) }
      .suchThat { x => BigInt(0) <= x && x <= len }
      .map (_ + min)
  }

  implicit val chooseSafeLong: Choose[SafeLong] = Choose.xmap(SafeLong(_:BigInt), _.toBigInt)

  // --- Cogen instances ---

  implicit val cogenSafeLong: Cogen[SafeLong] = Cogen.cogenLong.contramap(_.toLong)
  implicit val cogenRational: Cogen[Rational] = cogenSafeLong.contramap { r => r.numerator + r.denominator }

  // --- Diffable instances ---

  implicit def validatedDiffable[A: Diffable, E: Diffable]: Diffable[Validated[E, A]] = { (actual, expected) =>
    // todo to reimplement this with nice rendering
    val diffe = implicitly[Diffable[Either[E, A]]]
    diffe.diff(actual.toEither, expected.toEither)
  }

  // --- Numeric and its descendants instances ---

  implicit val rationalIsProbability: Fractional[Rational] = new Fractional[Rational] {
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
}

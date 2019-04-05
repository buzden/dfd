package ru.buzden

import cats.data.Validated
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.{Cogen, Gen}
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

  implicit val rationalIsProbability: Fractional[Rational] =
    ru.buzden.testing.util.numeric.instances.fractionalForRational

  implicit val safeLongIsIntegral: Integral[SafeLong] =
    ru.buzden.testing.util.numeric.instances.integralForSafeLong
}

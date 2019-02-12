package ru.buzden.testing
package probability

import cats.syntax.apply._
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.matcher.describe.Diffable
import ru.buzden.probability.dfd._
import ru.buzden.util.numeric.instances.scala2catsOrdering
import ru.buzden.util.numeric.syntax.zero
import spire.math.{Rational, SafeLong}

package object dfd {
  type SafeLongGen = Short
  def positivePowerOf2: Gen[SafeLongGen] = chooseNum(0, 8) `map` (1 << _) `map` (_.toShort)

  // --- Gen-related utility functions ---

  // standard denominator for this test set
  def denominator: Gen[SafeLongGen] = positivePowerOf2

  def rational(numerator: Gen[SafeLongGen]): Gen[Rational] = (numerator, denominator).mapN { Rational(_, _) }

  val posRational: Gen[Rational] = rational(posNum[SafeLongGen])
  val nonNegRational: Gen[Rational] = rational(nonNegNum[SafeLongGen])
  def between0and1: Gen[Rational] = for {
    den <- denominator
    num <- choose(zero[SafeLongGen], den)
  } yield Rational(num, den)

  def listOfNWithNonZero[A: Numeric](n: Int, genA: Gen[A]): Gen[List[A]] =
    listOfN(n, genA) `suchThat` { _.exists(_ =!= zero[A]) }

  // --- Other utility functions ---

  def normalize(l: List[Rational]): List[Rational] = {
    val sum = l.sum
    l `map` (_ / sum)
  }

  // --- Instances for testing ---

  implicit val arbSafeLong: Arbitrary[SafeLong] = Arbitrary(arbitrary[SafeLongGen] `map` { SafeLong(_) })

  implicit def cogen4dfd[A: Cogen:Ordering, P: Cogen]: Cogen[DiscreteFiniteDistribution[A, P]] =
    Cogen.cogenVector[(A, P)].contramap { dfd =>
      dfd.support.toVector.sorted.map(a => (a, dfd.pmf(a)))
    }

  implicit def dfdDiffable[A, P]: Diffable[DiscreteFiniteDistribution[A, P]] = { (actual, expected) =>
    def dfd2map(dfd: DiscreteFiniteDistribution[A, P]): Map[A, P] =
      Map(dfd.support.toList.map(a => (a, dfd.pmf(a))):_*)
    val diffm = implicitly[Diffable[Map[A, P]]]
    diffm.diff(dfd2map(actual), dfd2map(expected))
  }
}

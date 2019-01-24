package ru.buzden.probability.dfd

import cats.syntax.apply._
import cats.syntax.eq._
import org.scalacheck.Gen
import org.scalacheck.Gen.{Choose, chooseNum, frequency, listOfN, nonEmptyListOf, posNum}
import org.scalacheck.cats.implicits._
import ru.buzden.util.numeric.syntax.{one, zero}
import spire.math.Rational

object genUtil {
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
}

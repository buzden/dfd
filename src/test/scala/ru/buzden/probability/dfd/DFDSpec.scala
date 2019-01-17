package ru.buzden.probability.dfd

import cats.Apply
import cats.kernel.laws.discipline.EqTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import spire.math.Rational

class DFDSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  $eqLaws
  """

  import ru.buzden.probability.dfd.testInstances._

  implicit val cogenDfdIR: Cogen[DiscreteFiniteDistribution[Int, Rational]] = implicitly

  private def eqLaws = {
    implicit val arbitraryDfdIR: Arbitrary[DiscreteFiniteDistribution[Int, Rational]] =
      Arbitrary(GenDFD.genProportional)

    checkAll("DiscreteFiniteDistribution",
      EqTests[DiscreteFiniteDistribution[Int, Rational]].eqv
    )
  }
}

object GenDFD {
  def genProportional[A: Arbitrary, P: Probability]: Gen[DiscreteFiniteDistribution[A, P]] =
    nonEmptyListOf(Apply[Gen].product(arbitrary[A], posNum[Int])).map { l =>
      DiscreteFiniteDistribution.proportional(l.head, l.tail:_*)
    }.suchThat(_.isDefined).map(_.get)
}
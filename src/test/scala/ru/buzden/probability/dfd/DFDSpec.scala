package ru.buzden.probability.dfd

import cats.Apply
import cats.kernel.laws.discipline.EqTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.matcher.MatchResult
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.probability.dfd.testInstances._
import spire.math.Rational

class DFDSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  correctness of creation and created distributions
    general cases
      with normalized map
      with support and PMF
      proportional
      unnormalized
    particular distributions
      bernouli
      binomial
        correctness of support
        probabilities (a special case)
      hypergeometric
        correctness of support
        probabilities (a special case)
      uniform
  $eqLaws
  eagerization preserves support and probabilities
  """

  implicit val cogenDfdIR: Cogen[DiscreteFiniteDistribution[Int, Rational]] = implicitly

  private def eqLaws = {
    implicit val arbitraryDfdIR: Arbitrary[DiscreteFiniteDistribution[Int, Rational]] =
      proportionalCase[Int].arb

    checkAll("DiscreteFiniteDistribution",
      EqTests[DiscreteFiniteDistribution[Int, Rational]].eqv
    )
  }

  private def creationAndCorrectness[A, P](c: CaseDescription[A, P]) = {
    forAllNoShrink(c.genopt)(_ must beSome) // todo to conjunt the check from the case
  }

  trait CaseDescription[A, P] {
    type Intermediate
    type DFD = DiscreteFiniteDistribution[A, P]
    type CheckResult

    def intermediate: Gen[Intermediate]
    def createDfd: Intermediate => Option[DFD]
    def checkDfd: (Intermediate, DFD) => MatchResult[CheckResult]

    def genopt: Gen[Option[DFD]] = intermediate `map` createDfd
    def gen: Gen[DFD] = genopt.suchThat(_.isDefined).map(_.get)
    def arb: Arbitrary[DFD] = Arbitrary(gen)
  }

  def proportionalCase[A: Arbitrary]: CaseDescription[A, Rational] = new CaseDescription[A, Rational] {
    type Intermediate = List[(A, Int)]
    type CheckResult = Rational

    def intermediate: Gen[Intermediate] = nonEmptyListOf(Apply[Gen].product(arbitrary[A], posNum[Int]))

    def createDfd: Intermediate => Option[DFD] = { l =>
      DiscreteFiniteDistribution.proportional(l.head, l.tail: _*)
    }

    def checkDfd: (Intermediate, DFD) => MatchResult[CheckResult] = (ps, dfd) => {
      val matches = for {
        (a1, p1) <- ps
        (a2, p2) <- ps
      } yield Rational(p1, p2) ==== dfd.pmf(a1) / dfd.pmf(a2)

      matches.reduce(_ and _)
    }
  }
}
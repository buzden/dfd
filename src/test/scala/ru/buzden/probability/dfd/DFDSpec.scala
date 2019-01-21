package ru.buzden.probability.dfd

import cats.Apply
import cats.kernel.laws.discipline.EqTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.matcher.MatchResult
import org.specs2.specification.core.Fragments
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.probability.dfd.testInstances._
import spire.math.Rational

class DFDSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  correctness of creation and created distributions
    general cases
      with normalized map
      with support and PMF
      ${proportionalCase[String].fragments}
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

  trait TestCase {
    val caseName: String
    def fragments: Fragments
  }

  trait DfdGenCase[A, P] extends TestCase {
    type Intermediate
    type DFD = DiscreteFiniteDistribution[A, P]

    def intermediate: Gen[Intermediate]
    def checkSupport: (Intermediate, Set[A]) => MatchResult[_]

    def gen: Gen[(Intermediate, DFD)]
    def arb: Arbitrary[DFD] = Arbitrary(gen.map(_._2))
  }

  trait DfdGenOptCase[A, P] extends DfdGenCase[A, P] {
    def createDfd: Intermediate => Option[DFD]

    def genopt: Gen[(Intermediate, Option[DFD])] = intermediate `map` { x => (x, createDfd(x)) }
    override def gen: Gen[(Intermediate, DFD)] =
      genopt `suchThat` (_._2.isDefined) `map` { case (i, o) => (i, o.get) }

    def fragments = s2"""
      $caseName
        always creates properly   ${forAllNoShrink(genopt.map(_._2))(_ must beSome)}
        correctness of support    ${forAllNoShrink(gen){ case (i, d) => checkSupport(i, d.support)}}
        $probabilitiesFragments
      """

    protected def probabilitiesFragments: Fragments
  }

  trait DfdGenOptGeneralCase[A, P] extends DfdGenOptCase[A, P] {
    def checkProbabilities: (Intermediate, DFD) => MatchResult[_]

    protected override def probabilitiesFragments = s2"probabilities ${
      forAllNoShrink(gen)(checkProbabilities.tupled)}"
  }

  trait DfdGenOptSpecialCase[A, P] extends DfdGenOptCase[A, P] {
    def checkProbabilities: MatchResult[_]

    protected override def probabilitiesFragments = s2"probabilities (a special case) $checkProbabilities"
  }

  def proportionalCase[A: Arbitrary]: DfdGenCase[A, Rational] = new DfdGenOptGeneralCase[A, Rational] {
    type Intermediate = List[(A, Int)]
    type CheckResult = Rational

    override val caseName = "proportional"

    override def intermediate: Gen[Intermediate] =
      nonEmptyListOf(Apply[Gen].product(arbitrary[A], posNum[Int]))

    override def createDfd: Intermediate => Option[DFD] = { l =>
      DiscreteFiniteDistribution.proportional(l.head, l.tail: _*)
    }

    override def checkSupport: (List[(A, Int)], Set[A]) => MatchResult[_] = (l, s) =>
      s ==== l.map(_._1).toSet

    override def checkProbabilities: (Intermediate, DFD) => MatchResult[_] = (ps, dfd) => {
      val matches = for {
        (a1, p1) <- ps
        (a2, p2) <- ps
      } yield Rational(p1, p2) ==== dfd.pmf(a1) / dfd.pmf(a2)

      matches.reduce(_ and _)
    }
  }
}
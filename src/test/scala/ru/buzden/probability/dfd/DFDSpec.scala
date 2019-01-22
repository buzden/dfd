package ru.buzden.probability.dfd

import cats.kernel.laws.discipline.EqTests
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAllNoShrink
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

  // --- Particular DFD generation and checks cases ---

  def proportionalCase[A: Arbitrary]: TestCase[A, Rational] = new CanCheckAllProbabilities[A, Rational] {
    type DistrParameters = List[(A, Int)]
    type CheckResult = Rational

    override val caseName = "proportional"

    override def distrParameters: Gen[DistrParameters] =
      // todo to use analogue of `.distinct` based on `cats.Eq`.
      nonEmptyListOf(arbitrary[A]) `map` (_.distinct) `flatMap` { as =>
        listOfN(as.size, posNum[Int]) `map` { is => as `zip` is }
      }

    override def createDfd(l: DistrParameters): Option[Distr] =
      DiscreteFiniteDistribution.proportional(l.head, l.tail: _*)

    override def checkSupport(l: DistrParameters, s: Set[A]): MatchResult[_] =
      s ==== l.map(_._1).toSet

    override def checkProbabilities(ps: DistrParameters, dfd: Distr): MatchResult[_] = {
      val matches = for {
        (a1, p1) <- ps
        (a2, p2) <- ps
      } yield Rational(p1, p2) ==== dfd.pmf(a1) / dfd.pmf(a2)

      matches.reduce(_ and _)
    }
  }

  // --- Auxiliary classes for organization of test cases ---

  trait TestCase[A, P] {
    type DistrParameters
    type Distr = DiscreteFiniteDistribution[A, P]

    val caseName: String
    def fragments: Fragments

    def distrParameters: Gen[DistrParameters]
    def checkSupport(p: DistrParameters, support: Set[A]): MatchResult[_]

    def gen: Gen[(DistrParameters, Distr)]
    def arb: Arbitrary[Distr] = Arbitrary(gen.map(_._2))
  }

  trait OptionalCreationCase[A, P] extends TestCase[A, P] {
    def createDfd(p: DistrParameters): Option[Distr]

    def genopt: Gen[(DistrParameters, Option[Distr])] = distrParameters `map` { x => (x, createDfd(x)) }
    override def gen: Gen[(DistrParameters, Distr)] =
      genopt `suchThat` (_._2.isDefined) `map` { case (i, o) => (i, o.get) }

    def fragments = s2"""
      $caseName
        always creates properly   ${forAllNoShrink(genopt.map(_._2))(_ must beSome)}
        correctness of support    ${forAllNoShrink(gen){ case (i, d) => checkSupport(i, d.support)}}
        $probabilitiesFragments
      """

    protected def probabilitiesFragments: Fragments
  }

  trait CanCheckAllProbabilities[A, P] extends OptionalCreationCase[A, P] {
    def checkProbabilities(p: DistrParameters, d: Distr): MatchResult[_]

    protected override def probabilitiesFragments = s2"probabilities ${
      forAllNoShrink(gen) { (checkProbabilities _).tupled }
    }"
  }

  trait CanCheckOnlySpecialCase[A, P] extends OptionalCreationCase[A, P] {
    def checkProbabilities: MatchResult[_]

    protected override def probabilitiesFragments = s2"probabilities (a special case) $checkProbabilities"
  }
}
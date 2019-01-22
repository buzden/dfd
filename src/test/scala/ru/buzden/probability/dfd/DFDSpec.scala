package ru.buzden.probability.dfd

import cats.kernel.laws.discipline.EqTests
import cats.syntax.apply._
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
      ${normalizedMapCase[String].fragments}
      ${supportAndPmfCase[String].fragments}
      ${proportionalCase[String].fragments}
      ${unnormalizedCase[String].fragments}
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

  private val posRational: Gen[Rational] = (posNum[Long], posNum[Long]).mapN(Rational.apply)

  private def nonEmptyListOfDistinct[A](genA: Gen[A]): Gen[List[A]] =
    // todo to use analogue of `.distinct` based on `cats.Eq`.
    nonEmptyListOf(genA) `map` (_.distinct)

  private def normalize(l: List[Rational]) = {
    val sum = l.sum
    l `map` (_ / sum)
  }

  // --- Particular DFD generation and checks cases ---

  def normalizedMapCase[A: Arbitrary]: TestCase[A, Rational] = new CanCheckAllProbabilities[A, Rational] {
    override type DistrParameters = Map[A, Rational]
    override val caseName: String = "normalized map"

    override val distrParameters: Gen[DistrParameters] =
      nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
        listOfN(as.size, posRational) `map` normalize `map` (as `zip` _) `map` { Map(_:_*) }
      }

    override def createDfd(m: DistrParameters): Option[Distr] = DiscreteFiniteDistribution(m)

    override def checkSupport(m: DistrParameters, support: Set[A]): MatchResult[_] = support ==== m.keySet

    override def checkProbabilities(m: DistrParameters, d: Distr): MatchResult[_] =
      m `map` { case (a, p) => d.pmf(a) ==== p } `reduce` (_ and _)
  }

  def supportAndPmfCase[A: Arbitrary:Cogen]: TestCase[A, Rational] = new CanCheckAllProbabilities[A, Rational] {
    override type DistrParameters = (Set[A], A => Rational)
    override val caseName: String = "with support and PMF"

    override val distrParameters: Gen[DistrParameters] = {
      implicit val arbitraryRationalPositive: Arbitrary[Rational] = Arbitrary(posRational)
      for {
        support <- nonEmptyListOfDistinct(arbitrary[A])
        f <- arbitrary[A => Rational]
      } yield {
        val sum = support.map(f).sum
        (support.toSet, f `andThen` {_ / sum})
      }
    }

    override def createDfd(sf: DistrParameters): Option[Distr] = DiscreteFiniteDistribution(sf._1)(sf._2)

    override def checkSupport(sf: DistrParameters, support: Set[A]): MatchResult[_] = support ==== sf._1

    override def checkProbabilities(sf: DistrParameters, d: Distr): MatchResult[_] =
      sf._1.toList `map` { a => sf._2(a) ==== d.pmf(a) } `reduce` (_ and _)
  }

  def proportionalCase[A: Arbitrary]: TestCase[A, Rational] =
    new ProportionalLike[A, Int](
      "proportional",
      DiscreteFiniteDistribution.proportional,
      posNum[Int],
      Rational(_, _))

  def unnormalizedCase[A: Arbitrary]: TestCase[A, Rational] =
    new ProportionalLike[A, Rational](
      "unnormalized",
      DiscreteFiniteDistribution.unnormalized,
      posRational,
      _ / _)

  class ProportionalLike[A: Arbitrary, I](
    val caseName: String,
    val create: ((A, I), (A, I)*) => Option[DiscreteFiniteDistribution[A, Rational]],
    val genP: Gen[I],
    val div: (I, I) => Rational

    ) extends CanCheckAllProbabilities[A, Rational] {

    type DistrParameters = List[(A, I)]
    type CheckResult = Rational

    override val distrParameters: Gen[DistrParameters] =
      nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
        listOfN(as.size, genP) `map` { is => as `zip` is }
      }

    override def createDfd(l: DistrParameters): Option[Distr] =
      create(l.head, l.tail: _*)

    override def checkSupport(l: DistrParameters, s: Set[A]): MatchResult[_] =
      s ==== l.map(_._1).toSet

    override def checkProbabilities(ps: DistrParameters, dfd: Distr): MatchResult[_] = {
      val matches = for {
        (a1, p1) <- ps
        (a2, p2) <- ps
      } yield div(p1, p2) ==== dfd.pmf(a1) / dfd.pmf(a2)

      matches.reduce(_ and _)
    }
  }

  // --- Auxiliary classes for organization of test cases ---

  trait TestCase[A, P] {
    type DistrParameters
    type Distr = DiscreteFiniteDistribution[A, P]

    val caseName: String
    def fragments: Fragments

    val distrParameters: Gen[DistrParameters]
    def checkSupport(p: DistrParameters, support: Set[A]): MatchResult[_]

    val gen: Gen[(DistrParameters, Distr)]
    lazy val arb: Arbitrary[Distr] = Arbitrary(gen.map(_._2))
  }

  trait OptionalCreationCase[A, P] extends TestCase[A, P] {
    def createDfd(p: DistrParameters): Option[Distr]

    lazy val genopt: Gen[(DistrParameters, Option[Distr])] = distrParameters `map` { x => (x, createDfd(x)) }
    override lazy val gen: Gen[(DistrParameters, Distr)] =
      genopt `suchThat` (_._2.isDefined) `map` { case (i, o) => (i, o.get) }

    override def fragments = s2"""
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
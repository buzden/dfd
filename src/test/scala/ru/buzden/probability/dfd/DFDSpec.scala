package ru.buzden.probability.dfd

import cats.data.NonEmptySet
import cats.kernel.laws.discipline.EqTests
import cats.syntax.apply._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen._
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.matcher.MatchResult
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.probability.dfd.testInstances._
import ru.buzden.util.numeric.syntax._
import spire.math.Rational

import scala.collection.immutable.SortedSet

//noinspection TypeAnnotation
object DFDSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  correctness of creation and created distributions
    general cases
      ${normalizedMapCase[String].fragments}
      ${supportAndPmfCase[String].fragments}
      ${proportionalCase[String].fragments}
      ${unnormalizedCase[String].fragments}
    particular distributions
      ${bernouliCase.fragments}
      binomial
        correctness of support
        probabilities (a special case)
      hypergeometric
        correctness of support
        probabilities (a special case)
      ${uniformCase[String].fragments}
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

  private def nonNegNum[N: Numeric:Choose]: Gen[N] = frequency(1 -> zero[N], 99 -> posNum[N])

  private def rational(numerator: Gen[Long]): Gen[Rational] = (numerator, posNum[Long]).mapN(Rational.apply)

  private val posRational: Gen[Rational] = rational(posNum[Long])
  private val nonNegRational: Gen[Rational] = rational(nonNegNum[Long])
  private def between0and1[N: Numeric:Choose]: Gen[N] = chooseNum(zero[N], one[N])

  private def nonEmptyListOfDistinct[A](genA: Gen[A]): Gen[List[A]] =
    // todo to use analogue of `.distinct` based on `cats.Eq`.
    nonEmptyListOf(genA) `map` (_.distinct)

  private def normalize(l: List[Rational]) = {
    val sum = l.sum
    l `map` (_ / sum)
  }

  // --- Particular DFD generation and checks cases ---

  def normalizedMapCase[A: Arbitrary] = TestCase[A, Rational, Map[A, Rational]](
    caseName = "normalized map",
    distrParameters = nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
      listOfN(as.size, posRational) `map` normalize `map` (as `zip` _) `map` { Map(_:_*) }
    },
    createDfd = DiscreteFiniteDistribution(_),
    checkSupport = (m, support) => support ==== m.keySet,
    checkProbabilities = Left { (m, d) =>
      m `map` { case (a, p) => d.pmf(a) ==== p } `reduce` (_ and _)
    }
  )

  def supportAndPmfCase[A: Arbitrary:Cogen] = TestCase[A, Rational, (Set[A], A => Rational)](
    caseName = "with support and PMF",

    distrParameters = {
      implicit val arbitraryRationalPositive: Arbitrary[Rational] = Arbitrary(posRational)
      for {
        support <- nonEmptyListOfDistinct(arbitrary[A])
        f <- arbitrary[A => Rational]
      } yield {
        val sum = support.map(f).sum
        (support.toSet, f `andThen` {_ / sum})
      }
    },

    createDfd = sf => DiscreteFiniteDistribution(sf._1)(sf._2),
    checkSupport = (sf, support) => support ==== sf._1,

    checkProbabilities = Left { (sf, d) =>
      sf._1.toList `map` { a => sf._2(a) ==== d.pmf(a) } `reduce` (_ and _)
    }
  )

  def proportionalCase[A: Arbitrary] =
    proportionalLike[A, Int](
      "proportional",
      DiscreteFiniteDistribution.proportional,
      posNum[Int],
      Rational(_, _))

  def unnormalizedCase[A: Arbitrary] =
    proportionalLike[A, Rational](
      "unnormalized",
      DiscreteFiniteDistribution.unnormalized,
      posRational,
      _ / _)

  private def proportionalLike[A: Arbitrary, I](
    caseN: String,
    create: ((A, I), (A, I)*) => Option[DiscreteFiniteDistribution[A, Rational]],
    genP: Gen[I],
    div: (I, I) => Rational
  ) = TestCase[A, Rational, List[(A, I)]](
    caseName = caseN,

    distrParameters = nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
      listOfN(as.size, genP) `map` { is => as `zip` is }
    },

    createDfd = l => create(l.head, l.tail: _*),
    checkSupport = (l, s) => s ==== l.map(_._1).toSet,

    checkProbabilities = Left { (ps, dfd) =>
      val matches = for {
        (a1, p1) <- ps
        (a2, p2) <- ps
      } yield div(p1, p2) ==== dfd.pmf(a1) / dfd.pmf(a2)

      matches.reduce(_ and _)
    }
  )

  // --- Particular distribution cases ---

  lazy val bernouliCase = TestCase[Boolean, Rational, Rational](
    caseName = "bernouli",
    distrParameters = between0and1[Rational],
    createDfd = DiscreteFiniteDistribution.bernouli,
    checkSupport = (_, support) => support must not be empty,
    checkProbabilities = Left { (p, d) =>
      (d.pmf(true) ==== p) and (d.pmf(false) ==== (1 - p))
    }
  )

  def uniformCase[A: Arbitrary:Ordering] = TestCase[A, Rational, NonEmptySet[A]](
    caseName = "uniform",

    distrParameters =
      nonEmptyListOfDistinct(arbitrary[A]) `map` { SortedSet[A](_:_*) } `map` { NonEmptySet.fromSetUnsafe },

    createDfd = s => Some(DiscreteFiniteDistribution.uniform(s)),
    checkSupport = (s, support) => support ==== s.toSortedSet,

    checkProbabilities = Left { (s, d) =>
      val expectedP = Rational(1, s.length)
      d.support.toList `map` (d.pmf(_) ==== expectedP) `reduce` (_ and _)
    }
  )

  // --- Auxiliary classes for organization of test cases ---

  final case class TestCase[A, P, Param](
    caseName: String,
    distrParameters: Gen[Param],
    createDfd: Param => Option[DiscreteFiniteDistribution[A, P]],
    checkSupport: (Param, Set[A]) => MatchResult[_],
    checkProbabilities: Either[(Param, DiscreteFiniteDistribution[A, P]) => MatchResult[_], MatchResult[_]],
  ) {
    type DistrParameters = Param
    type Distr = DiscreteFiniteDistribution[A, P]

    lazy val genopt: Gen[(DistrParameters, Option[Distr])] = distrParameters `map` { x => (x, createDfd(x)) }
    lazy val gen: Gen[(DistrParameters, Distr)] =
      genopt `suchThat` (_._2.isDefined) `map` { case (i, o) => (i, o.get) }
    lazy val arb: Arbitrary[Distr] = Arbitrary(gen.map(_._2))

    def fragments(implicit A: Arbitrary[A], P: Numeric[P]) = s2"""
      $caseName
        always creates properly            ${forAllNoShrink(genopt.map(_._2))(_ must beSome)}
        correctness of support set         ${forAllNoShrink(gen){ case (i, d) => checkSupport(i, d.support)}}
        pmf != zero when in support        $pmfNonZeroWhenInSupport
        pmf == zero when not in support    $pmfIsZeroWhenNotInSupport
        $probabilitiesFragments
      """

    private def pmfNonZeroWhenInSupport(implicit A: Arbitrary[A], P: Numeric[P]) =
      forAllNoShrink(gen) { case (_, d) =>
        d.support.toList `map` (d.pmf(_) !=== zero[P]) `reduce` (_ and _)
      }

    private def pmfIsZeroWhenNotInSupport(implicit A: Arbitrary[A], P: Numeric[P]) =
      forAllNoShrink(gen) { case (_, d) =>
        forAllNoShrink(arbitrary[A] `filterNot` d.support) { notInSupport =>
          d.pmf(notInSupport) ==== zero[P]
        }
      }

    private def probabilitiesFragments = checkProbabilities match {
      case Left(checkF)  => s2"probabilities ${forAllNoShrink(gen) { checkF.tupled } }"
      case Right(result) => s2"probabilities (a special case) $result"
    }
  }
}
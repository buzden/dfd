package ru.buzden.probability.dfd

import cats.Apply
import cats.data.NonEmptySet
import cats.kernel.laws.discipline.EqTests
import cats.syntax.apply._
import cats.syntax.eq._
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
import spire.math.{Rational, SafeLong}

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
      ${binomialCase.fragments}
      ${hypergeometricCase.fragments}
      ${uniformCase[String].fragments}
    relation between different distributions
      bernouli(1/2)  == uniform for booleans                                 $bernouliOfHalf
      binomial(1, p) ~= bernouli(p)
    eagerization preserves support and probabilities                         $eagerizationPreserves
  laws of typeclass instances
    $eqLaws
  """

  // --- Thing-in-ifself-like checks ---

  def eqLaws = checkAll("DiscreteFiniteDistribution",
    EqTests[DiscreteFiniteDistribution[Int, Rational]].eqv
  )

  // --- Particular DFD generation and checks cases ---

  def normalizedMapCase[A: Arbitrary] = TestCase[A, Rational, Map[A, Rational]](
    caseName = "normalized map",
    distrParameters = nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
      listOfNWithNonZero(as.size, nonNegRational) `map` normalize `map` (as `zip` _) `map` { Map(_:_*) }
    },
    createDfd = DiscreteFiniteDistribution(_),
    checkSupport = (m, support) => support ==== m.filter(_._2 =!= zero[Rational]).keySet,
    checkProbabilities = { (m, d) =>
      m `map` { case (a, p) => d.pmf(a) ==== p } `reduce` (_ and _)
    }
  )

  def supportAndPmfCase[A: Arbitrary:Cogen] = TestCase[A, Rational, (Set[A], A => Rational)](
    caseName = "with support and PMF",

    distrParameters = {
      implicit val _: Arbitrary[Rational] = Arbitrary(nonNegRational)
      for {
        support <- nonEmptyListOfDistinct(arbitrary[A])
        f <- arbitrary[A => Rational]
      } yield {
        val sum = support.map(f).sum
        (support.toSet, f `andThen` {_ / sum})
      }
    },

    createDfd = sf => DiscreteFiniteDistribution(sf._1)(sf._2),
    checkSupport = { case ((s, f), support) => support ==== s.filter { f(_) =!= zero[Rational] } },

    checkProbabilities = { case ((s, f), d) =>
      s.toList `map` { a => f(a) ==== d.pmf(a) } `reduce` (_ and _)
    }
  )

  def proportionalCase[A: Arbitrary] =
    proportionalLike[A, Int](
      "proportional",
      DiscreteFiniteDistribution.proportional,
      nonNegNum[Int],
      Rational(_, _))

  def unnormalizedCase[A: Arbitrary] =
    proportionalLike[A, Rational](
      "unnormalized",
      DiscreteFiniteDistribution.unnormalized,
      nonNegRational,
      _ / _)

  private def proportionalLike[A: Arbitrary, I: Numeric](
    caseN: String,
    create: ((A, I), (A, I)*) => Option[DiscreteFiniteDistribution[A, Rational]],
    genP: Gen[I],
    div: (I, I) => Rational,
  ) = TestCase[A, Rational, List[(A, I)]](
    caseName = caseN,

    distrParameters = nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
      listOfNWithNonZero(as.size, genP) `map` { is => as `zip` is }
    },

    createDfd = l => create(l.head, l.tail: _*),
    checkSupport = (l, s) => s ==== l.filter(_._2 =!= zero[I]).map(_._1).toSet,

    checkProbabilities = { (ps, dfd) =>
      val psnn = ps.filter(_._2 =!= zero[I])
      val matches = for {
        (a1, p1) <- psnn
        (a2, p2) <- psnn
      } yield div(p1, p2) ==== dfd.pmf(a1) / dfd.pmf(a2)

      matches.reduce(_ and _)
    }
  )

  def eagerizationPreserves = forAllNoShrink(anyDfd) { dfd =>
    DiscreteFiniteDistribution.eager(dfd) ==== dfd
  }

  // --- Particular distribution cases ---

  lazy val bernouliCase = TestCase[Boolean, Rational, Rational](
    caseName = "bernouli",
    distrParameters = between0and1[Rational],
    createDfd = DiscreteFiniteDistribution.bernouli,
    checkSupport = (_, support) => support must not be empty,
    checkProbabilities = { (p, d) =>
      (d.pmf(true) ==== p) and (d.pmf(false) ==== (1 - p))
    }
  )

  private def factorial(n: Int): SafeLong = (1 to n).map { SafeLong(_) }.product
  private def binomial(n: Int, k: Int): SafeLong = factorial(n) / (factorial(n - k) * factorial(k))

  lazy val binomialCase = TestCase[Int, Rational, (Int, Rational)](
    caseName = "binomial",
    distrParameters = Apply[Gen].product(nonNegNum[Int], between0and1[Rational]),
    createDfd = (DiscreteFiniteDistribution.binomial[Rational, Int] _).tupled,
    checkSupport = (np, support) => support ==== (0 to np._1).toSet,
    checkProbabilities = { case ((n, p), d) =>
      def bin(k: Int): Rational = binomial(n, k) * p.pow(k) * (one[Rational] - p).pow(n - k)
      (0 to n) `map` { k => d.pmf(k) ==== bin(k) } `reduce` (_ and _)
    }
  )

  private def hypergeometricSupport(nn: Int, kk: Int, n: Int): Range = (0 `max` n + kk - nn) to (nn `min` kk)
  lazy val hypergeometricCase = TestCase[Int, Rational, (Int, Int, Int)](
    caseName = "hypergeometric",
    distrParameters = for {
      nn <- nonNegNum[Int]
      kk <- chooseNum(0, nn)
      n <- chooseNum(0, nn)
    } yield (nn, kk, n),
    createDfd = { case (nn, kk, n) => DiscreteFiniteDistribution.hypergeometric(nn, kk, n) },
    checkSupport = { case ((nn, kk, n), support) =>
      support ==== hypergeometricSupport(nn, kk, n).toSet
    },
    checkProbabilities = { case ((nn, kk, n), d) =>
      def p(k: Int): Rational = binomial(kk, k) * binomial(nn - kk, n - k) / binomial(nn, n)
      hypergeometricSupport(nn, kk, n) `map` { k => d.pmf(k) ==== p(k) } `reduce` (_ and _)
    }
  )

  def uniformCase[A: Arbitrary:Ordering] = TestCase[A, Rational, NonEmptySet[A]](
    caseName = "uniform",

    distrParameters =
      nonEmptyListOfDistinct(arbitrary[A]) `map` { SortedSet[A](_:_*) } `map` { NonEmptySet.fromSetUnsafe },

    createDfd = s => Some(DiscreteFiniteDistribution.uniform(s)),
    checkSupport = (s, support) => support ==== s.toSortedSet,

    checkProbabilities = { (s, d) =>
      val expectedP = Rational(1, s.length)
      d.support.toList `map` (d.pmf(_) ==== expectedP) `reduce` (_ and _)
    }
  )

  def bernouliOfHalf = {
    import DiscreteFiniteDistribution._
    bernouli(Rational(1, 2)) ==== Some(uniform(NonEmptySet.of(true, false)))
  }

  // --- Auxiliary classes for organization of test cases ---

  final case class TestCase[A, P, Param](
    caseName: String,
    distrParameters: Gen[Param],
    createDfd: Param => Option[DiscreteFiniteDistribution[A, P]],
    checkSupport: (Param, Set[A]) => MatchResult[_],
    checkProbabilities: (Param, DiscreteFiniteDistribution[A, P]) => MatchResult[_],
  ) {
    type DistrParameters = Param
    type Distr = DiscreteFiniteDistribution[A, P]

    lazy val genopt: Gen[(DistrParameters, Option[Distr])] = distrParameters `map` { x => (x, createDfd(x)) }
    lazy val gen: Gen[(DistrParameters, Distr)] =
      genopt `suchThat` (_._2.isDefined) `map` { case (i, o) => (i, o.get) }
    lazy val genD: Gen[Distr] = gen.map(_._2)

    def fragments(implicit A: Arbitrary[A], P: Numeric[P]) = s2"""
      $caseName
        always creates properly            ${forAllNoShrink(genopt.map(_._2))(_ must beSome)}

        pmf != zero when in support        ${forAllNoShrink(genD)(pmfNonZeroWhenInSupport)}
        pmf == zero when not in support    ${forAllNoShrink(genD)(pmfIsZeroWhenNotInSupport)}
        sum of all probabilities is one    ${forAllNoShrink(genD)(pmfSumIsOne)}

        correctness of support set         ${forAllNoShrink(gen){ case (i, d) => checkSupport(i, d.support) }}
        values of probabilities            ${forAllNoShrink(gen)(checkProbabilities.tupled)}
      """

    private def pmfNonZeroWhenInSupport(d: Distr)(implicit P: Numeric[P]) =
      d.support.toList `map` (d.pmf(_) !=== zero[P]) `reduce` (_ and _)

    private def pmfIsZeroWhenNotInSupport(d: Distr)(implicit A: Arbitrary[A], P: Numeric[P]) =
      forAllNoShrink(arbitrary[A] `filterNot` d.support) { notInSupport =>
        d.pmf(notInSupport) ==== zero[P]
      }

    private def pmfSumIsOne(d: Distr)(implicit P: Numeric[P]) =
      d.support.toList.map(d.pmf).sum ==== one[P]
  }

  // --- General purpose utility functions and values ---

  implicit val cogenDfdIR: Cogen[DiscreteFiniteDistribution[Int, Rational]] = implicitly

  implicit lazy val arbDfdIR: Arbitrary[DiscreteFiniteDistribution[Int, Rational]] = Arbitrary(anyDfd)
  lazy val anyDfd: Gen[DiscreteFiniteDistribution[Int, Rational]] = Gen.oneOf(
    normalizedMapCase[Int].genD,
    supportAndPmfCase[Int].genD,
    proportionalCase[Int].genD,
    unnormalizedCase[Int].genD,
    // no bernouli case until it's for booleans
    binomialCase.genD,
    hypergeometricCase.genD,
    uniformCase[Int].genD,
  )

  private def nonNegNum[N: Numeric:Choose]: Gen[N] = frequency(1 -> zero[N], 99 -> posNum[N])

  private def rational(numerator: Gen[Long]): Gen[Rational] = (numerator, posNum[Long]).mapN(Rational.apply)

  private val posRational: Gen[Rational] = rational(posNum[Long])
  private val nonNegRational: Gen[Rational] = rational(nonNegNum[Long])
  private def between0and1[N: Numeric:Choose]: Gen[N] = chooseNum(zero[N], one[N])

  private def listOfNWithNonZero[A: Numeric](n: Int, genA: Gen[A]): Gen[List[A]] =
    listOfN(n, genA) `suchThat` { _.exists(_ =!= zero[A]) }

  private def nonEmptyListOfDistinct[A](genA: Gen[A]): Gen[List[A]] =
  // todo to use analogue of `.distinct` based on `cats.Eq`.
    nonEmptyListOf(genA) `map` (_.distinct)

  private def normalize(l: List[Rational]): List[Rational] = {
    val sum = l.sum
    l `map` (_ / sum)
  }
}
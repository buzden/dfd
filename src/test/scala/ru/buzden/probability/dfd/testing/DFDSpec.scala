package ru.buzden.probability.dfd.testing

import cats.Apply
import cats.data.Validated.Valid
import cats.data.{NonEmptySet, ValidatedNel}
import cats.kernel.laws.discipline.EqTests
import cats.laws.discipline.MonadTests
import cats.laws.discipline.SemigroupalTests.Isomorphisms
import cats.syntax.eq._
import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.chooseNum
import org.scalacheck.Prop.forAllNoShrink
import org.scalacheck.cats.implicits._
import org.scalacheck.{Arbitrary, Cogen, Gen}
import org.specs2.matcher.MatchResult
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.probability.dfd.DiscreteFiniteDistribution._
import ru.buzden.probability.dfd._
import ru.buzden.util.numeric.syntax.{one, zero}
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
      ${bernouliCase[Int].fragments}
      ${binomialCase.fragments}
      ${hypergeometricCase.fragments}
      ${uniformCase[String].fragments}
    relation between different distributions
      bernouli(1/2)  == uniform for booleans                                 ${bernouliOfHalf[Int]}
      binomial(1, p) == bernouli(p)                                          $binomialOfOne
      hypergeometric(N, K, 1) == bernouli(K/N)                               $hypergeometricWithN1
    preservation of support and probabilities
      on eagerization                                                        ${preserves[SafeLong](eagerify)}
      on lazification                                                        ${preserves[SafeLong](lazify)}
  $eqLaws
  $monadLaws
  """

  type V[A] = ValidatedNel[String, A]

  // --- Thing-in-ifself-like checks ---

  def eqLaws = checkAll("DiscreteFiniteDistribution",
    EqTests[DiscreteFiniteDistribution[SafeLong, Rational]].eqv
  )

  type DfdRational[A] = DiscreteFiniteDistribution[A, Rational] // This type is a workaround of compiler bug
  def monadLaws(implicit wtf: Isomorphisms[DfdRational]) = checkAll("DiscreteFiniteDistribution",
    MonadTests[DfdRational].monad[SafeLong, String, SafeLong]
  )

  // --- Particular DFD generation and checks cases ---

  def normalizedMapCase[A: Arbitrary] = TestCase[A, Rational, Map[A, Rational]](
    caseName = "normalized map",
    distrParameters = nonEmptyListOfDistinct(arbitrary[A]) `flatMap` { as =>
      listOfNWithNonZero(as.size, nonNegRational) `map` normalize `map` (as `zip` _) `map` { Map(_:_*) }
    },
    createDfd = DiscreteFiniteDistribution[A, Rational, V],
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
        sum = support.map(f).sum
        if sum =!= zero[Rational]
      } yield (support.toSet, f `andThen` {_ / sum})
    },

    createDfd = sf => DiscreteFiniteDistribution[A, Rational, V](sf._1)(sf._2),
    checkSupport = (_, support) => support must not be empty,

    checkProbabilities = { case ((s, f), d) =>
      s.toList `map` { a => f(a) ==== d.pmf(a) } `reduce` (_ and _)
    }
  )

  def proportionalCase[A: Arbitrary] =
    proportionalLike[A, Int](
      "proportional",
      proportional[A, Rational, V],
      nonNegNum[Int],
      Rational(_, _))

  def unnormalizedCase[A: Arbitrary] =
    proportionalLike[A, Rational](
      "unnormalized",
      unnormalized[A, Rational, V],
      nonNegRational,
      _ / _)

  private def proportionalLike[A: Arbitrary, I: Numeric](
    caseN: String,
    create: ((A, I), (A, I)*) => V[DiscreteFiniteDistribution[A, Rational]],
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

  def preserves[A](f: DiscreteFiniteDistribution[A, Rational] => DiscreteFiniteDistribution[A, Rational])(implicit arbD: Arbitrary[DiscreteFiniteDistribution[A, Rational]]) =
    forAllNoShrink { dfd: DiscreteFiniteDistribution[A, Rational] =>
      f(dfd) ==== dfd
    }

  // --- Particular distribution cases ---

  def bernouliCase[N: Numeric] = TestCase[N, Rational, Rational](
    caseName = "bernouli",
    distrParameters = between0and1[Rational],
    createDfd = bernouli[N, Rational, V],
    checkSupport = (_, support) => support must not be empty,
    checkProbabilities = { (p, d) =>
      (d.pmf(one[N]) ==== p) and (d.pmf(zero[N]) ==== (1 - p))
    }
  )

  private def factorial(n: Int): SafeLong = (1 to n).map { SafeLong(_) }.product
  private def binomialCoef(n: Int, k: Int): Rational = Rational(factorial(n), factorial(n - k) * factorial(k))

  private def binomialSupport(n: Int, p: Rational): Set[Int] =
    if (p === zero[Rational]) Set(0)
    else if (p === one[Rational]) Set(n)
    else (0 to n).toSet
  lazy val binomialCase = TestCase[SafeLong, Rational, (Int, Rational)](
    caseName = "binomial",
    distrParameters = Apply[Gen].product(nonNegNum[Int], between0and1[Rational]),
    createDfd = { case (n, p) => binomial[SafeLong, Rational, V](SafeLong(n), p) },
    checkSupport = (np, support) => support ==== binomialSupport(np._1, np._2).map { SafeLong(_:Int) },
    checkProbabilities = { case ((n, p), d) =>
      def bin(k: Int): Rational = binomialCoef(n, k) * p.pow(k) * (one[Rational] - p).pow(n - k)
      (0 to n) `map` { k => d.pmf(k) ==== bin(k) } `reduce` (_ and _)
    }
  )

  private def hypergeometricSupport(nn: Int, kk: Int, n: Int): Range = (0 `max` n + kk - nn) to (n `min` kk)
  lazy val hypergeometricCase = TestCase[SafeLong, Rational, (Int, Int, Int)](
    caseName = "hypergeometric",
    distrParameters = for {
      nn <- nonNegNum[Int]
      kk <- chooseNum(0, nn)
      n <- chooseNum(0, nn)
    } yield (nn, kk, n),
    createDfd = { case (nn, kk, n) => hypergeometric[SafeLong, Rational, V](SafeLong(nn), SafeLong(kk), SafeLong(n)) },
    checkSupport = { case ((nn, kk, n), support) =>
      support ==== hypergeometricSupport(nn, kk, n).toSet.map { SafeLong(_:Int) }
    },
    checkProbabilities = { case ((nn, kk, n), d) =>
      def p(k: Int): Rational = binomialCoef(kk, k) * binomialCoef(nn - kk, n - k) / binomialCoef(nn, n)
      hypergeometricSupport(nn, kk, n) `map` { k => d.pmf(k) ==== p(k) } `reduce` (_ and _)
    }
  )

  def uniformCase[A: Arbitrary:Ordering] = TestCase[A, Rational, NonEmptySet[A]](
    caseName = "uniform",
    distrParameters =
      nonEmptyListOfDistinct(arbitrary[A]) `map` { SortedSet[A](_:_*) } `map` { NonEmptySet.fromSetUnsafe },
    createDfd = s => Valid(uniform(s)),
    checkSupport = (s, support) => support ==== s.toSortedSet,
    checkProbabilities = { (s, d) =>
      val expectedP = Rational(1, s.length)
      d.support.toList `map` (d.pmf(_) ==== expectedP) `reduce` (_ and _)
    }
  )

  def bernouliOfHalf[N: Numeric] =
    bernouli[N, Rational, V](Rational(1, 2)) ==== Valid(uniform(NonEmptySet.of(zero[N], one[N])))

  def binomialOfOne = forAllNoShrink(between0and1[Rational]) { p =>
    binomial[SafeLong, Rational, V](one[SafeLong], p) ==== bernouli[SafeLong, Rational, V](p)
  }

  def hypergeometricWithN1 = forAllNoShrink(Gen.posNum[SafeLong]) { nn =>
    forAllNoShrink(chooseNum(one[SafeLong], nn)) { kk =>
      hypergeometric[SafeLong, Rational, V](nn, kk, one[SafeLong]) ====
        bernouli[SafeLong, Rational, V](Rational(kk, nn))
    }
  }

  // --- Auxiliary classes for organization of test cases ---

  final case class TestCase[A, P, Param](
    caseName: String,
    distrParameters: Gen[Param],
    createDfd: Param => V[DiscreteFiniteDistribution[A, P]],
    checkSupport: (Param, Set[A]) => MatchResult[_],
    checkProbabilities: (Param, DiscreteFiniteDistribution[A, P]) => MatchResult[_],
  ) {
    type DistrParameters = Param
    type Distr = DiscreteFiniteDistribution[A, P]

    lazy val genopt: Gen[(DistrParameters, V[Distr])] = distrParameters `map` { x => (x, createDfd(x)) }
    lazy val gen: Gen[(DistrParameters, Distr)] =
      genopt `suchThat` (_._2.isValid) `map` { case (i, o) => (i, o.toOption.get) }
    lazy val genD: Gen[Distr] = gen.map(_._2)

    def fragments(implicit A: Arbitrary[A], P: Numeric[P]) = s2"""
      $caseName
        always creates properly            ${forAllNoShrink(genopt)(_ must beLike { case (_, Valid(_)) => ok })}

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

  // --- Putting all cases together ---

  implicit lazy val arbDfdIR: Arbitrary[DiscreteFiniteDistribution[SafeLong, Rational]] = Arbitrary(Gen.oneOf(
    normalizedMapCase[SafeLong].genD,
    supportAndPmfCase[SafeLong].genD,
    proportionalCase[SafeLong].genD,
    unnormalizedCase[SafeLong].genD,
    bernouliCase[SafeLong].genD,
    binomialCase.genD,
    hypergeometricCase.genD,
    uniformCase[SafeLong].genD,
  ))

  implicit def arbDfdAny[A: Arbitrary:Cogen]: Arbitrary[DiscreteFiniteDistribution[A, Rational]] =
    Arbitrary(Gen.oneOf(
      normalizedMapCase[A].genD,
      supportAndPmfCase[A].genD,
      proportionalCase[A].genD,
      unnormalizedCase[A].genD,
    ))
}

package ru.buzden.probability.dfd

import cats.arrow.ArrowChoice
import cats.data.{NonEmptyList, NonEmptySet}
import cats.instances.list._
import cats.instances.map._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.order._
import cats.{ApplicativeError, Eq, Monad, Order}
import ru.buzden.util.numeric.instances.scala2catsOrdering
import ru.buzden.util.numeric.syntax._

import scala.Fractional.Implicits._
import scala.Integral.Implicits._
import scala.annotation.tailrec

sealed trait DiscreteFiniteDistribution[A, P] {
  /** Probability mass function.
    * Returns zero for arguments that are not in the support.
    */
  def pmf: A => P

  /** Distribution's support, i.e. a set of arguments on which pmf gives non-zero */
  def support: Set[A] // todo must be a non-empty set

  /** Cumulative distribution function */
  def cdf(implicit O: Order[A], N: Numeric[P]): A => P = a =>
    support.filter(_ <= a).map(pmf).sum
}

// --- Discrete finite distributions implementations ---

private final class MapDFD[A, P: Probability](pmfRaw: => Map[A, P])
  extends DiscreteFiniteDistribution[A, P]  {
  override lazy val pmf: Map[A, P] = pmfRaw `filter` { case (_, p) => p =!= zero } `withDefaultValue` zero
  override lazy val support: Set[A] = pmf.keySet
}

private object MapDFD {
  def apply[A, P: Probability](pmfRaw: => Map[A, P]): DiscreteFiniteDistribution[A, P] = new MapDFD(pmfRaw)
  def unapply[A, P](mDfd: MapDFD[A, P]): Option[Map[A, P]] = Some(mDfd.pmf)
}

private final class FunctionDFD[A, P: Probability](supportRaw: => Set[A], pmfRaw: A => P)
  extends DiscreteFiniteDistribution[A, P] {
  override lazy val pmf: A => P = a => if (support(a)) pmfRaw(a) else zero
  override lazy val support: Set[A] = supportRaw `filter` { pmfRaw(_) =!= zero }
}

private object FunctionDFD {
  def apply[A, P: Probability](supportRaw: => Set[A], pmfRaw: A => P): DiscreteFiniteDistribution[A, P] =
    new FunctionDFD[A, P](supportRaw, pmfRaw)
  def unapply[A, P](fDfd: FunctionDFD[A, P]): Option[(Set[A], A => P)] = Some((fDfd.support, fDfd.pmf))
}

object DiscreteFiniteDistribution {
  type Errorable[Container[_]] = ApplicativeError[Container, NonEmptyList[String]]
  private def check[E[_]: Errorable](failMsg: => String)(v: Boolean): E[Unit] =
    if (!v) NonEmptyList.one(failMsg).raiseError[E, Unit] else ().pure[E]
  private def checkEq[E[_]: Errorable, A: Eq](desc: => String, actual: A, expected: A): E[Unit] =
    check(s"$desc is $actual but must be equal to $expected") { actual === expected }

  // --- Discrete finite distribution creation variants ---

  def apply[A, P: Probability, E[_]: Errorable](pmf: Map[A, P]): E[DiscreteFiniteDistribution[A, P]] =
    check[E]("A probability value that is <= zero exists") { pmf.values.forall(_ >= zero) } *>
    checkEq("Sum of all probabilities", pmf.values.sum, one) *>
    MapDFD(pmf).pure[E]

  def apply[A, P: Probability, E[_]: Errorable](support: Set[A])(pmf: A => P): E[DiscreteFiniteDistribution[A, P]] =
    check[E]("A probability value that is <= zero exists") { support.forall(pmf(_) >= zero) } *>
    checkEq("Sum of all probabilities", support.toSeq.map(pmf).sum, one) *>
    FunctionDFD(support, pmf).pure[E]

  def proportional[A, P: Probability, E[_]: Errorable](p1: (A, Int), rest: (A, Int)*): E[DiscreteFiniteDistribution[A, P]] = {
    def pairToProb(p: (A, Int)): (A, P) = p.copy(_2 = p._2.asNumeric)
    unnormalized[A, P, E](pairToProb(p1), rest `map` pairToProb :_*)
  }

  def unnormalized[A, P: Probability, E[_]: Errorable](p1: (A, P), rest: (A, P)*): E[DiscreteFiniteDistribution[A, P]] = {
    import ru.buzden.util.numeric.instances.numericAdditiveMonoid
    val ps = p1 :: rest.toList
    val sum = ps.foldMap(_._2)
    check[E]("Sum of probabilities is equal to zero") { sum =!= zero } *>
    DiscreteFiniteDistribution[A, P, E](ps `foldMap` { case (a, p) => Map(a -> p / sum) })
  }

  def eagerify[A, P: Probability](dfd: DiscreteFiniteDistribution[A, P]): DiscreteFiniteDistribution[A, P] = dfd match {
    case m@MapDFD(_) => m
    case FunctionDFD(support, pmf) => MapDFD(Map(support.map { a => a -> pmf(a) }.toSeq:_*))
  }

  def lazify[A, P: Probability](dfd: DiscreteFiniteDistribution[A, P]): DiscreteFiniteDistribution[A, P] =
    dfd match {
      case MapDFD(m) => FunctionDFD(m.keySet, m)
      case f@FunctionDFD(_, _) => f
    }

  // --- Examples of discrete finite distributions ---

  def bernouli[N: Numeric, P: Probability, E[_]: Errorable](p: P): E[DiscreteFiniteDistribution[N, P]] =
    check[E](s"Bernouli parameter P=$p must be between 0 and 1") { p >= zero[P] && p <= one[P] } *>
    DiscreteFiniteDistribution[N, P, E](Map(one[N] -> p, zero[N] -> (one[P] - p)))

  def binomial[N: Integral, P: Probability, E[_]: Errorable](n: N, p: P)(implicit ntop: N => P): E[DiscreteFiniteDistribution[N, P]] =
    check[E](s"Binomial coefficient P=$p must be in [0, 1]") { p >= zero[P] && p <= one[P] } *>
    DiscreteFiniteDistribution[N, P, E]((zero[N] to n).toSet) { k =>
      p.pow(k) * (one[P] - p).pow(n - k) * n.combinationsI(k)
    }

  def hypergeometric[N: Integral, P: Probability, E[_]: Errorable](N: N, K: N, n: N)(implicit ntop: N => P): E[DiscreteFiniteDistribution[N, P]] =
    check[E](s"N=$N must be non-negative") { N >= zero[N] } *>
    check[E](s"K=$K must lie between 0 and N=$N") { K >= zero[N] && K <= N } *>
    check[E](s"n=$n must lie between 0 and N=$N") { n >= zero[N] && n <= N } *>
    DiscreteFiniteDistribution[N, P, E](((zero[N] `max` n + K - N) `to` (n `min` K)).toSet) { k =>
      ntop(K.combinationsI(k) * (N - K).combinationsI(n - k)) / ntop(N.combinationsI(n))
    }

  def uniform[A, P: Probability](support: NonEmptySet[A]): DiscreteFiniteDistribution[A, P] = {
    val p = one / support.length.asNumeric
    FunctionDFD(support.toSortedSet, { _ => p })
  }

  // --- cats.Eq instance ---

  implicit def dfdEq[A, P: Probability]: Eq[DiscreteFiniteDistribution[A, P]] = (d1, d2) =>
    // Supports are equal
    d1.support.forall(d2.support) && d2.support.forall(d1.support) &&
      // PMF are equal on given support
      d1.support.forall(x => d1.pmf(x) === d2.pmf(x))

  // --- cats.Monad instance ---

  implicit def dfdMonad[P: Probability]: Monad[DiscreteFiniteDistribution[?, P]] = new Monad[DiscreteFiniteDistribution[?, P]] {
    /** Just a shorter type alias having the `P` type inside */
    type DFD[X] = DiscreteFiniteDistribution[X, P]

    override def pure[A](x: A): DFD[A] = FunctionDFD(Set(x), _ => one[P])

    private def dfd2aps[A](dfd: DFD[A]): List[(A, P)] = dfd match {
      case MapDFD(m) => m.toList
      case FunctionDFD(s, f) => s.toList `map` { a => a -> f(a) }
    }

    import ru.buzden.util.numeric.instances.numericAdditiveMonoid // for Semigroup[P]

    /** Builds DFD from list of possibly repeating "a"s with probabilities.
      * It is assumed that sum of all probabilities is one.
      */
    private def aps2dfd[A](aps: List[(A, P)]): DFD[A] =
      MapDFD(aps `foldMap` { Map(_) })

    // todo to treat function DFDs in the lazy manner (if it's possible)
    override def map[A, B](fa: DFD[A])(f: A => B): DFD[B] = {
      val ms = dfd2aps(fa) `map` { case (a, p) => (f(a), p) }
      aps2dfd(ms)
    }

    // todo to treat function DFDs in the lazy manner (if it's possible)
    override def flatMap[A, B](fa: DFD[A])(f: A => DFD[B]): DFD[B] = {
      val ms = for {
        (a, p) <- dfd2aps(fa)
        (b, q) <- dfd2aps(f(a))
      } yield b -> p * q
      aps2dfd(ms)
    }

    override def tailRecM[A, B](a: A)(f: A => DFD[A Either B]): DFD[B] = {
      @tailrec def tailRecImpl(curr: DFD[A Either B]): DFD[B] =
        if (curr.support `forall` { _.isRight }) map(curr)(_.toOption.get)
        else {
          val next = flatMap(curr) {
            case r@Right(_) => pure(r: A Either B)
            case Left(aa) => f(aa)
          }
          tailRecImpl(next)
        }
      tailRecImpl(f(a))
    }
  }

  // --- cats.arrow.Arrow instance ---

  final case class ProbabilisticComputation[A, B, P](comp: A => DiscreteFiniteDistribution[B, P]) extends AnyVal

  implicit def dfdArrow[P: Probability] = new ArrowChoice[ProbabilisticComputation[?, ?, P]] {
    /** Just a shorter type alias having the `P` type inside */
    type ~=>[A, B] = ProbabilisticComputation[A, B, P]

    override def lift[A, B](f: A => B): A ~=> B = ???

    override def first[A, B, C](fa: A ~=> B): (A, C) ~=> (B, C) = ???

    override def compose[A, B, C](f: B ~=> C, g: A ~=> B): A ~=> C = ???

    override def choose[A, B, C, D](f: A ~=> C)(g: B ~=> D): Either[A, B] ~=> Either[C, D] = ???
  }
}
package ru.buzden.probability.dfd

import cats.data.NonEmptySet
import cats.instances.list._
import cats.instances.map._
import cats.syntax.foldable._
import cats.syntax.order._
import cats.{Eq, Order}
import ru.buzden.util.numeric.syntax._

import scala.Fractional.Implicits._
import scala.Integral.Implicits._

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

object DiscreteFiniteDistribution {
  // --- Discrete finite distributions implementations ---

  private final case class MapDFD[A, P](pmf: Map[A, P]) extends DiscreteFiniteDistribution[A, P]  {
    override def support: Set[A] = pmf.keySet
  }

  private final case class FunctionDFD[A, P: Probability](pmfBase: A => P, support: Set[A])
    extends DiscreteFiniteDistribution[A, P] {
    override def pmf: A => P = a => if (support(a)) pmfBase(a) else zero
  }

  // --- Discrete finite distribution creation variants ---

  def apply[A, P: Probability](pmf: Map[A, P]): Option[DiscreteFiniteDistribution[A, P]] =
    if (pmf.values.forall(_ >= zero) && (pmf.values.sum === one))
      Some(MapDFD(pmf `filter` { case (_, p) => p =!= zero } `withDefaultValue` zero)) else None

  def apply[A, P: Probability](support: Set[A])(pmf: A => P): Option[DiscreteFiniteDistribution[A, P]] =
    if (support.forall(pmf(_) >= zero) && (support.toSeq.map(pmf).sum === one))
      Some(FunctionDFD(pmf, support `filter` { pmf(_) =!= zero })) else None

  def proportional[A, P: Probability](p1: (A, Int), rest: (A, Int)*): Option[DiscreteFiniteDistribution[A, P]] = {
    def pairToProb(p: (A, Int)): (A, P) = p.copy(_2 = p._2.asNumeric)
    unnormalized(pairToProb(p1), rest `map` pairToProb :_*)
  }

  def unnormalized[A, P: Probability](p1: (A, P), rest: (A, P)*): Option[DiscreteFiniteDistribution[A, P]] = {
    import ru.buzden.util.numeric.instances.numericAdditiveMonoid
    val ps = p1 :: rest.toList
    val sum = ps.foldMap(_._2)
    if (sum =!= zero)
      DiscreteFiniteDistribution(ps `foldMap` { case (a, p) => Map(a -> p / sum) })
    else None
  }

  def eager[A, P](dfd: DiscreteFiniteDistribution[A, P]): DiscreteFiniteDistribution[A, P] = dfd match {
    case m@MapDFD(_) => m
    case FunctionDFD(pmf, support) => MapDFD(Map(support.map { a => a -> pmf(a) }.toSeq:_*))
  }

  // --- Examples of discrete finite distributions ---

  def bernouli[P: Probability](p: P): Option[DiscreteFiniteDistribution[Boolean, P]] =
    if (p >= zero && p <= one) DiscreteFiniteDistribution(Map(true -> p, false -> (one - p))) else None

  def binomial[N: Integral, P: Probability](n: N, p: P)(implicit ntop: N => P): Option[DiscreteFiniteDistribution[N, P]] =
    if (p >= zero[P] && p <= one[P]) DiscreteFiniteDistribution((zero[N] to n).toSet) { k =>
      p.pow(k) * (one[P] - p).pow(n - k) * n.combinationsI(k)
    } else None

  def hypergeometric[N: Integral, P: Probability](N: N, K: N, n: N)(implicit ntop: N => P): Option[DiscreteFiniteDistribution[N, P]] =
    if (N >= zero[N] && K >= zero[N] && K <= N && n >= zero[N] && n <= N)
      DiscreteFiniteDistribution(((zero[N] `max` n + K - N) `to` (n `min` K)).toSet) { k =>
        K.combinationsI(k) * (N - K).combinationsI(n - k) / N.combinationsI(n)
      }
    else None

  def uniform[A, P: Probability](support: NonEmptySet[A]): DiscreteFiniteDistribution[A, P] = {
    val p = one / support.length.asNumeric
    FunctionDFD({ _ => p }, support.toSortedSet)
  }

  // --- cats.Eq instance ---

  implicit def dfdEq[A, P: Probability]: Eq[DiscreteFiniteDistribution[A, P]] = (d1, d2) =>
    // Supports are equal
    d1.support.forall(d2.support) && d2.support.forall(d1.support) &&
      // PMF are equal on given support
      d1.support.forall(x => d1.pmf(x) === d2.pmf(x))
}
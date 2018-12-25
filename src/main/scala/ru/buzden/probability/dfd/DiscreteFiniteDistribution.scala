package ru.buzden.probability.dfd

import cats.Order
import cats.syntax.order._

final case class DiscreteFiniteDistribution[A, P] private (
  /** Probability mass function */
  pmf: Map[A, P]
) {
  /** Distribution's support, i.e. a set of arguments on which pmf gives non-zero */
  def support: Set[A] = pmf.keySet

  /** Cumulative distribution function */
  def cdf(implicit O: Order[A], F: Fractional[P]): A => P = a =>
    support.filter(_ <= a).map(pmf).sum
}

object DiscreteFiniteDistribution {
  @inline private def zero[P: Fractional] = implicitly[Fractional[P]].zero
  @inline private def one[P: Fractional] = implicitly[Fractional[P]].one
  private implicit def scala2catsOrdering[A: Ordering]: Order[A] = Order.fromOrdering

  def apply[A, P: Fractional](pmf: Map[A, P]): Option[DiscreteFiniteDistribution[A, P]] = {
    if (pmf.values.forall(_ >= zero) && (pmf.values.sum === one))
      Some(DiscreteFiniteDistribution(pmf filter { case (_, p) => p =!= zero })) else None
  }
}
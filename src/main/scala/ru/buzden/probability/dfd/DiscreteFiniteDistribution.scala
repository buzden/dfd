package ru.buzden.probability.dfd

import cats.Order
import cats.syntax.order._

sealed trait DiscreteFiniteDistribution[A, P] {
  /** Probability mass function */
  def pmf: A => P

  /** Distribution's support, i.e. a set of arguments on which pmf gives non-zero */
  def support: Set[A]

  /** Cumulative distribution function */
  def cdf(implicit O: Order[A], F: Fractional[P]): A => P = a =>
    support.filter(_ <= a).map(pmf).sum
}

object DiscreteFiniteDistribution {
  @inline private def zero[P: Fractional] = implicitly[Fractional[P]].zero
  @inline private def one[P: Fractional] = implicitly[Fractional[P]].one
  private implicit def scala2catsOrdering[A: Ordering]: Order[A] = Order.fromOrdering

  private final case class MapDFD[A, P](pmf: Map[A, P]) extends DiscreteFiniteDistribution[A, P]  {
    override def support: Set[A] = pmf.keySet
  }

  private final case class FunctionDFD[A, P](pmf: A => P, support: Set[A])
    extends DiscreteFiniteDistribution[A, P]

  def apply[A, P: Fractional](pmf: Map[A, P]): Option[DiscreteFiniteDistribution[A, P]] =
    if (pmf.values.forall(_ >= zero) && (pmf.values.sum === one))
      Some(MapDFD(pmf filter { case (_, p) => p =!= zero })) else None

  def apply[A, P: Fractional](support: Set[A])(pmf: A => P): Option[DiscreteFiniteDistribution[A, P]] =
    if (support.forall(pmf(_) >= zero) && (support.toSeq.map(pmf).sum === one))
      Some(FunctionDFD(pmf, support filter { pmf(_) =!= zero })) else None
}
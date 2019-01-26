package ru.buzden.probability.dfd

import cats.data.{NonEmptyList, NonEmptySet}
import cats.instances.list._
import cats.instances.map._
import cats.syntax.applicative._
import cats.syntax.applicativeError._
import cats.syntax.apply._
import cats.syntax.foldable._
import cats.syntax.order._
import cats.{Applicative, ApplicativeError, Eq, Order}
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

  private[dfd] def pure[F[_]: Applicative]: F[DiscreteFiniteDistribution[A, P]] =
    this.pure[F]
}

object DiscreteFiniteDistribution {
  type Errorable[Container[_]] = ApplicativeError[Container, NonEmptyList[String]]
  private def check[E[_]: Errorable](failMsg: String)(v: => Boolean): E[Unit] =
    if (v) NonEmptyList.one(failMsg).raiseError[E, Unit] else ().pure[E]

  implicit def prodolbErrorOption[E]: ApplicativeError[Option, E] = new ApplicativeError[Option, E] {
    override def raiseError[A](e: E): Option[A] = None

    override def handleErrorWith[A](fa: Option[A])(f: E => Option[A]): Option[A] = fa

    override def pure[A](x: A): Option[A] = Some(x)

    import cats.instances.option._
    override def ap[A, B](ff: Option[A => B])(fa: Option[A]): Option[B] = Applicative[Option].ap(ff)(fa)
  }

  // --- Discrete finite distributions implementations ---

  private final case class MapDFD[A, P](pmf: Map[A, P]) extends DiscreteFiniteDistribution[A, P]  {
    override def support: Set[A] = pmf.keySet
  }

  private final case class FunctionDFD[A, P: Probability](pmfBase: A => P, support: Set[A])
    extends DiscreteFiniteDistribution[A, P] {
    override def pmf: A => P = a => if (support(a)) pmfBase(a) else zero
  }

  // --- Discrete finite distribution creation variants ---

  def apply[A, P: Probability, E[_]: Errorable](pmf: Map[A, P]): E[DiscreteFiniteDistribution[A, P]] =
    check[E]("A probability value that is <= zero exists") { pmf.values.forall(_ >= zero) } *>
    check[E]("Sum of all probabilities is not equal to one") { pmf.values.sum === one } *>
    MapDFD(pmf `filter` { case (_, p) => p =!= zero } `withDefaultValue` zero).pure[E]

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
      DiscreteFiniteDistribution[A, P, Option](ps `foldMap` { case (a, p) => Map(a -> p / sum) })
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
        ntop(K.combinationsI(k) * (N - K).combinationsI(n - k)) / ntop(N.combinationsI(n))
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
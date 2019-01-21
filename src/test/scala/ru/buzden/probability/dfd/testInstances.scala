package ru.buzden.probability.dfd

import org.scalacheck.{Cogen, Gen}
import spire.math.Rational

object testInstances {
  implicit val rationalIsProbability: Probability[Rational] = new Fractional[Rational] {
    override def plus(x: Rational, y: Rational): Rational = x + y
    override def minus(x: Rational, y: Rational): Rational = x - y

    override def times(x: Rational, y: Rational): Rational = x * y
    override def div(x: Rational, y: Rational): Rational = x / y

    override def negate(x: Rational): Rational = -x

    override def compare(x: Rational, y: Rational): Int = x `compare` y

    override def fromInt(x: Int): Rational = Rational(x)

    override def toInt(x: Rational): Int = x.toInt
    override def toLong(x: Rational): Long = x.toLong
    override def toFloat(x: Rational): Float = x.toFloat
    override def toDouble(x: Rational): Double = x.toDouble
  }

  implicit def cogen4dfd[A: Cogen:Ordering, P: Cogen]: Cogen[DiscreteFiniteDistribution[A, P]] =
    Cogen.cogenVector[(A, P)].contramap { dfd =>
      dfd.support.toVector.sorted.map(a => (a, dfd.pmf(a)))
    }

  def unopt[A](gopt: Gen[Option[A]]): Gen[A] = gopt.suchThat(_.isDefined).map(_.get)
}

package ru.buzden.probability.dfd

import org.scalacheck.Arbitrary.arbitrary
import org.scalacheck.Gen.Choose
import org.scalacheck.{Cogen, Gen}
import spire.math.{Rational, SafeLong}

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

  implicit val safeLongIsNumeric: Numeric[SafeLong] = new Integral[SafeLong] {
    override def plus(x: SafeLong, y: SafeLong): SafeLong = x + y
    override def minus(x: SafeLong, y: SafeLong): SafeLong = x - y

    override def times(x: SafeLong, y: SafeLong): SafeLong = x * y

    override def quot(x: SafeLong, y: SafeLong): SafeLong = x / y
    override def rem(x: SafeLong, y: SafeLong): SafeLong = x % y

    override def negate(x: SafeLong): SafeLong = -x

    override def compare(x: SafeLong, y: SafeLong): Int = x `compare` y

    override def fromInt(x: Int): SafeLong = SafeLong(x)

    override def toInt(x: SafeLong): Int = x.toInt
    override def toLong(x: SafeLong): Long = x.toLong
    override def toFloat(x: SafeLong): Float = x.toFloat
    override def toDouble(x: SafeLong): Double = x.toDouble
  }

  implicit def cogen4dfd[A: Cogen:Ordering, P: Cogen]: Cogen[DiscreteFiniteDistribution[A, P]] =
    Cogen.cogenVector[(A, P)].contramap { dfd =>
      dfd.support.toVector.sorted.map(a => (a, dfd.pmf(a)))
    }

  implicit val chooseBigInt: Choose[BigInt] = (min, max) => {
    if (min > max) throw new Choose.IllegalBoundsError(min, max) // they originally throw :-(
    val len = max - min

    Gen.listOfN((len.bitLength + 7) / 8, arbitrary[Byte])
      .map { bs => BigInt(1, Array(bs:_*)) }
      .suchThat { x => BigInt(0) <= x && x <= len }
      .map (_ + min)
  }

  implicit val chooseSafeLong: Choose[SafeLong] = Choose.xmap(SafeLong(_:BigInt), _.toBigInt)

  // a_min * b_min <= x / y <= a_max / b_max
  // assuming denominators to be positive,
  // a_min * b_max * y <= b_min * b_max * x <= a_max * b_min * y
  //
  // This implementation assumes that Rational's denominators are positive
  implicit val chooseRational: Choose[Rational] = (min, max) => for {
    y <- Gen.posNum[SafeLong]
    u <- Gen.chooseNum(min.numerator * max.denominator * y, max.numerator * min.denominator * y)
  } yield Rational(u, y * min.denominator * max.denominator)
}

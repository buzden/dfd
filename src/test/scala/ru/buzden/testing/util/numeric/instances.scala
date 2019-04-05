package ru.buzden.testing.util.numeric

import spire.math.{Rational, SafeLong}

import scala.util.Try

object instances {
  val fractionalForRational: Fractional[Rational] = new Fractional[Rational] {
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

    override def parseString(str: String): Option[Rational] = Try(Rational(str)).toOption
  }

  val integralForSafeLong: Integral[SafeLong] = new Integral[SafeLong] {
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

    override def parseString(str: String): Option[SafeLong] = Numeric[Long].parseString(str).map(SafeLong(_))
  }
}

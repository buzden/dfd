package ru.buzden.testing
package util.numeric

import org.scalacheck.Prop.forAll
import org.specs2.{ScalaCheck, Specification}
import org.typelevel.discipline.specs2.Discipline
import ru.buzden.util.numeric.syntax._
import spire.math.Rational

//noinspection TypeAnnotation
object UtilNumericSpec extends Specification with ScalaCheck with Discipline { def is = s2"""
  factorial(0) == 1 (Int)                                                      ${fac0[Int]}
  factorial(0) == 1 (BigInt)                                                   ${fac0[BigInt]}
  combinations for Integral and Fractional corresponds to each other           $combinationsCorr
  """

  def fac0[N: Numeric] = zero[N].factorial ==== one[N]

  def combinationsCorr = forAll { (n: Short, k: Short) =>
    val (ni, ki) = (BigInt(n), BigInt(k))
    val (nr, kr) = (Rational(n), Rational(k))
    (nr `combinationsF` kr).toBigInt ==== (ni `combinationsI` ki)
  }
}
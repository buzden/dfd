package ru.buzden.util.numeric

import cats.syntax.eq._
import ru.buzden.util.numeric.instances.scala2catsOrdering

import scala.Ordering.Implicits._

object syntax {
  @inline def zero[P: Numeric]: P = implicitly[Numeric[P]].zero
  @inline def one[P: Numeric]: P = implicitly[Numeric[P]].one
  @inline def two[P: Numeric]: P = 2.asNumeric

  implicit class FromIntToNumericSyntax(val x: Int) extends AnyVal {
    def asNumeric[P: Numeric]: P = implicitly[Numeric[P]].fromInt(x)
  }

  implicit class ExtendedNumericSyntax[N](val n: N) extends AnyVal {
    import scala.Numeric.Implicits._
    def to(till: N)(implicit N: Numeric[N]): Iterator[N] =
      if (n <= till) Iterator.iterate(n)(_ + one).takeWhile(_ <= till)
      else Iterator.empty

    def factorial(implicit N: Numeric[N]): N = (one to n).product

    def sqr(implicit N: Numeric[N]): N = n * n
  }

  implicit class FractionalPowSyntax[N](val n: N) extends AnyVal {
    import scala.Fractional.Implicits._
    import scala.Integral.Implicits._

    // todo can be rewritten without non-tail recursion
    def pow[K: Integral](k: K)(implicit F: Fractional[N]): N =
      if (k === zero[K]) one[N]
      else if (k < zero[K]) one[N] / pow(-k)
      else if (k % two[K] === zero[K]) pow(k / two[K]).sqr
      else n * pow(k - one[K])
  }

  implicit class CombinationsSyntax[N](val n: N) extends AnyVal {
    import scala.Numeric.Implicits._
    private def c(k: N)(div: (N, N) => N)(implicit N: Numeric[N]): N =
      if (k === zero) one
      else if (n === k) one
      else if (k > n) zero
      else {
        val kk = k `max` n - k
        div((kk + one `to` n).product, (n - kk).factorial)
      }

    def combinationsI(k: N)(implicit N: Integral[N]): N = c(k)(N.quot)
    def combinationsF(k: N)(implicit N: Fractional[N]): N = c(k)(N.div)
  }
}

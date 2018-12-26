package ru.buzden.util

package object numeric {
  @inline def zero[P: Numeric]: P = implicitly[Numeric[P]].zero
  @inline def one[P: Numeric]: P = implicitly[Numeric[P]].one

  implicit class FromIntToNumericSyntax(val x: Int) extends AnyVal {
    def asNumeric[P: Numeric]: P = implicitly[Numeric[P]].fromInt(x)
  }
}

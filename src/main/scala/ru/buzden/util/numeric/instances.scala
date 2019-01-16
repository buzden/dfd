package ru.buzden.util.numeric

import cats.Monoid

object instances {
  implicit def numericAdditiveMonoid[A: Numeric]: Monoid[A] = new Monoid[A] {
    override def empty: A = implicitly[Numeric[A]].zero
    override def combine(x: A, y: A): A = implicitly[Numeric[A]].plus(x, y)
  }
}

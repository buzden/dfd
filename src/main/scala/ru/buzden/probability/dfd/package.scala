package ru.buzden.probability

import cats.Order

package object dfd {
  private[dfd] implicit def scala2catsOrdering[A: Ordering]: Order[A] = Order.fromOrdering

  // todo to make this to be a nice typeclass for probability
  type Probability[P] = Fractional[P]
}

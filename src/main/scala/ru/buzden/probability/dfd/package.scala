package ru.buzden.probability

package object dfd {
  // todo to make this to be a nice typeclass for probability
  type Probability[P] = Fractional[P]
}

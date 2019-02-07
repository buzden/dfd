## Discrete finite probability distributions for Scala

[![Build status](https://travis-ci.com/buzden/dfd.svg)](https://travis-ci.com/buzden/dfd)
[![Code coverage](https://codecov.io/github/buzden/dfd/coverage.svg?branch=master)](https://codecov.io/github/buzden/dfd)
[![Cats friendly](https://typelevel.org/cats/img/cats-badge-tiny.png)](https://typelevel.org/cats)

### Overview

The main goal of this library is to provide a data structure that can represent a probability distribution of (more or less) arbitrary objects.
Desired data structure should have the following properties:

- observability the whole distribution, i.e. having an ability to get probabilities not only by sampling;
- ability of precise probability values (not only with `Double`s);
- unability to represent incorrect distributions (e.g. when sum of probabilities is not equal to one);
- ability to combine computations which have distribution as a result;
- purity, e.g. support of the *purely functional* fashion of programming;
- representability as an *effect* (in the wide sense of effect in pure functional programming).

Some of these properties require us to consider only *finite* and *discrete* probability distributions.

### Installation

TBD

### Usage

TBD

### Related projects

There are several similar projects (or parts of bigger projects) which have more or less the same goals
but differing in taken decisions and implementation details.
Here is a (not complete!) list of such:

- [Probability distribution monad](https://github.com/jliszka/probability-monad)
- [ScalaNLP Breeze](https://github.com/scalanlp/breeze), particularly
  [`breeze.stats.distributions`](https://github.com/scalanlp/breeze/tree/master/math/src/main/scala/breeze/stats/distributions) package
  (see also [this blogpost](https://www.chrisstucchio.com/blog/2016/probability_the_monad.html))
- [Fast, Lightweight library for Information and Probablity](https://github.com/xxxnell/flip)
- [Scaladice](https://github.com/pdehn/Scaladice)
- [Noel Welsh's `pfennig`](https://github.com/noelwelsh/pfennig)

### Credits

This project was supported by and originally developed for the [ductilejur](https://forge.ispras.ru/projects/ductilejur) project.

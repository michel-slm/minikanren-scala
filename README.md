A Scala port of [miniKanren](http://minikanren.org/)
====================================================

[![Build Status](https://travis-ci.org/aborg0/minikanren-scala.svg?branch=sbt)](https://travis-ci.org/aborg0/minikanren-scala)
[![Coverage Status](https://coveralls.io/repos/github/aborg0/minikanren-scala/badge.svg?branch=sbt)](https://coveralls.io/github/aborg0/minikanren-scala?branch=sbt)

Based on https://github.com/michel-slm/minikanren-scala

Please check the documentation in [docs/presentation.rst](docs/presentation) for details.

Using REPL with SBT:

    > miniKanrenJVM/console

...

    scala> time(run(1, x)(solve_puzzle(x)))
    res0: (Long, Any) = (10044,List(List(9567, 1085, 10652)))

Interpretation of the result: took `10044` milliseconds to solve the problem, a result is

     SEND   9567
    +MORE  +1085
    -----  -----
    MONEY  10652

Another example (palindromes with six-digit numbers that are the product of two three-digit numbers), this time with `maprun` as it is much faster:

    time(maprun(1, x)(palprod_o(x)))
    100001
    101101
    res1: (Long, Any) = (40837,List((1,(1,(1,(0,(0,(1,(1,(1,(1,(1,(0,(0,(0,(1,List()))))))))))))))))

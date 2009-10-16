Porting the Mini-Kanren logic system to Scala
=============================================

:Author: Michel Alexandre Salim

.. image:: http://i.creativecommons.org/l/by-sa/3.0/us/88x31.png
   :height: 31px
   :width:  88px
   :alt:    Creative Commons License
   :align:  center

Abstract
--------

.. class:: incremental

Mini-Kanren is a simplified implementation of Kanren, a declarative
logic system, embedded in a pure functional subset of Scheme.

.. class:: incremental

This presentation describes a port to Scala, written for the graduate
programming language course at Indiana University.


Outline
-------

This presentation is in three sections:

1. `The Mini-Kanren logic system`_
2. `An overview of Scala`_
3. `The port`_

The Mini-Kanren logic system
----------------------------

To many ears, the term *logic programming* is virtually synonymous
with Prolog (see [Colmerauer92]_ for a historical treatment). Outside
the domain of Artificial Intelligence, computer science practicioners
tend not to be exposed to the field -- in most cases, students are
first exposed to procedural, then object-oriented, then functional
languages\ [*]_.

.. [Colmerauer92] *The birth of Prolog*, Colmerauer and Russell, 1992

.. class:: incremental

.. [*] If they are (un)lucky, functional comes first


Mini-Kanren: References
-----------------------

  The goal of *The Reasoned Schemer* is to help the functional
  programmer think logically and the logic programmer think
  functionally. -- [Friedman05]_

This presentation uses material sourced from the book, beta-tested by
several classes of IU computer science students.

.. [Friedman05] *The Reasoned Schemer*, by Daniel P. Friedman, William E. Byrd and Oleg Kiselyov

Mini-Kanren: Substitution
-------------------------

A *substitution* is a mapping from logical variables to values\
[#]_. It is immutable; extending a substitution with a new key-value
pair produces a new substitution, with the old substitution remaining
unchanged\ [#]_.

.. [#] including logical variables
.. [#] Satisfied by Scheme association lists, or Clojure persistent maps

Mini-Kanren: Goals
------------------

A *goal* is a function that, given a substitution, returns a stream of
substitutions. There are two basic goals:

.. class:: incremental

- **succeed** (**#s**) returns a stream containing only the input substitution
- **fail** (**#u**) returns an empty stream


Mini-Kanren: Conditionals
-------------------------

Four basic conditional constructs:

.. class:: incremental

- if\ :sup:`e` -- each goal can succeed
- if\ :sup:`i` -- each goal can succeed, output is interleaved
- if\ :sup:`a` -- a single line, cf. soft-cut. only one goal can succeed
- if\ :sup:`u` -- uni-. like if\ :sup:`a`, but the successful
  *question* only succeeds once

.. class:: incremental

We'll stick with if\ :sup:`e` first, and discuss the others in a bit

List predicate (Scheme)
-----------------------
::

  (def list?
    (λ (l)
      (if (null? l)
        #t
        (if (pair? l)
          (list? (cdr l))
          #f))))

A list is either an empty list, or a pair whose tail is a list


List predicate (Kanren)
-----------------------
    
::

  (def list°
    (λ (l)
      (ife (null° l)
        #s
        (ife (pair° l)
          (fresh° (d)
	    (cdr° l d)
            (list° d))
          #u))))

List predicates
---------------

Note the differences:

- if\ :sup:`e` instead of if
- cdr\ :sup:`o` instead of cdr
- relations cannot be used as function arguments
- relations return goals, not values

Mini-Kanren: infinite goals
---------------------------

::

  (define any°
    (λ (g)
      (ife g #s
           (any° g))))

  (define always° (any° #s))
  (define never°  (any° #u))



An overview of Scala
--------------------

  Scala is a concise, elegant, type-safe programming language that
  integrates object-oriented and functional features.\ [#]_


.. [#] http://www.scala-lang.org/

Scala: the name
---------------

  The name Scala stands for “scalable language.” The language is so
  named because it was designed to grow with the demands of its
  users. You can apply Scala to a wide range of programming tasks,
  from writing small scripts to building large systems.\ [#]_

.. [#] *Scala: A Scalable Language*, by Martin Odersky, Lex Spoon, and Bill Venners

Scala: the authors
------------------

Scala is developed by the `LAMP group`_ at EPFL, led by Prof. Martin
Odersky, who previously worked on `Pizza`_ and `Generic Java`_

.. _LAMP group: http://lamp.epfl.ch/
.. _Pizza: http://pizzacompiler.sourceforge.net/
.. _Generic Java: http://www.cis.unisa.edu.au/~pizza/gj/

Scala: Pros
-----------

.. class:: incremental

- runs on the JVM
- interoperates well with Java
- and thus with other JVM languages
- provides functional programming constructs
- pattern-matching
- powerful type system


Scala: Cons
-----------

Cons
~~~~

.. class:: incremental

- no mutual TCO (blame Sun)
- No macros
- call-by-name provides same power (but not conciseness)

Scala: Objects
--------------

Objects serve two purposes:

.. class:: incremental

- as a code container (cf. Python modules)
- as singletons

.. class:: incremental

Let's look at a concrete example

Scala: Objects (cont.)
----------------------

::

  package info.hircus.kanren
  object MiniKanren {
    import java.util.HashMap
    case class Var(name: Symbol, count: Int)
    private val m = new HashMap[Symbol, Int]()
    def make_var(name: Symbol) = {
      val count = m.get(name)
      m.put(name, count+1)
      Var(name, count)
    } /* more code */
  }

Scala: REPL
-----------

Scala provides a read-evaluate-print-loop interpreter, familiar to
users of functional and scripting languages

::

  scala> import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.MiniKanren._

  scala> val v = make_var('hello)
  v: info.hircus.kanren.MiniKanren.Var = Var('hello,0)

  scala> val w = make_var('hello)
  w: info.hircus.kanren.MiniKanren.Var = Var('hello,1)

Scala: REPL (cont.)
-------------------

REPL
~~~~

::

  scala> val v = make_var('hello)
  v: info.hircus.kanren.MiniKanren.Var = Var('hello,2)

  scala> v = make_var('world)
  <console>:7: error: reassignment to val
         v = make_var('world)

.. class:: incremental

Values cannot be reassigned -- use variables for that.

Scala: Pattern matching
-----------------------

Those familiar with either OCaml or Haskell will be right at home with Scala's pattern-matching construct.
Unlike Haskell, there is no pattern matching on function definitions.

.. class:: incremental

Contrast an implementation of a list-summing function in the three languages:

.. class:: incremental

::

  lsum :: (Num t) => [t] -> t -- this line is optional
  lsum [] = 0
  lsum (h:tl) = h + lsum tl


Scala: Pattern matching
-----------------------

.. class:: incremental

::

  # let rec sum list = match list with
    | [] -> 0
    | head::tail -> head + sum tail;;
  val sum : int list -> int = <fun>

.. class:: incremental

::

  scala> def sum(l: List[Int]): Int = l match {
       | case Nil => 0
       | case h::tl => h + sum(tl)
       | }
  sum: (List[Int])Int


Scala: scalacheck
-----------------

*scalacheck*\ [#]_ is a tool for random testing of program properties, with
 automatic test case generation. It was initially a port of Haskell's
 *QuickCheck*\ [#]_ library.

.. [#] http://code.google.com/p/scalacheck/
.. [#] http://hackage.haskell.org/package/QuickCheck-2.1.0.2

Scala: scalacheck examples
--------------------------

::

  import org.scalacheck._

  object StringSpecification extends Properties("String") {
    property("startsWith") = Prop.forAll((a: String, b: String) => (a+b).startsWith(a))
    // Is this really always true?
    property("concat") = Prop.forAll((a: String, b: String) => 
      (a+b).length > a.length && (a+b).length > b.length )
    property("substring") = Prop.forAll((a: String, b: String) => 
      (a+b).substring(a.length) == b )
  }

The port
--------

The initial port was done over the course of several weeks; the current implementation is a rewrite\ [#]_. The initial implementation had a stack-overflow bug
that was reëncountered during the rewrite, which I'll discuss in a bit.

.. class:: incremental

- better test coverage: using scalacheck
- better use of Scala features
- less "Scheme"-ish interface. e.g. use **if** instead of **cond**

.. [#] original code is lost. moral story: backup (and share online...)

mplus (Scheme)
--------------

::

  (define mplus
    (lambda (a-inf f)
      (case-inf a-inf
        (f) 
        ((a) (choice a f))
        ((a f0) (choice a 
                  (lambdaf@ () (mplus (f0) f)))))))

mplus (Scala)
-------------

::

  def mplus(a_inf: Stream[Subst],
            f: => Stream[Subst]): Stream[Subst] =
    a_inf append f

mplus\ :sup:`i` (Scheme)
------------------------

::

  (define mplusi
    (lambda (a-inf f)
      (case-inf a-inf
        (f) 
        ((a) (choice a f))
        ((a f0) (choice a 
                  (lambdaf@ () (mplusi (f) f0)))))))


mplus\ :sup:`i` (Scala)
-----------------------

::

  def mplus_i(a_inf: Stream[Subst],
            f: => Stream[Subst]): Stream[Subst] = a_inf match {
    case Stream.empty => f
    case Stream.cons(a, f0) => f0 match {
      case Stream.empty => Stream.cons(a, f)
      case _ => Stream.cons(a, mplus_i(f, f0))
    }
  }

bind (Scheme)
-------------

::

  (define bind
    (lambda (a-inf g)
      (case-inf a-inf
        (mzero)
        ((a) (g a))
        ((a f) (mplus (g a)
                 (lambdaf@ () (bind (f) g)))))))

bind (Scala)
------------

::

  def bind(a_inf: Stream[Subst], g: Goal): Stream[Subst] =
    a_inf flatMap g


bind\ :sup:`i` (Scheme)
-----------------------

::

  (define bindi
    (lambda (a-inf g)
      (case-inf a-inf
        (mzero)
        ((a) (g a))
        ((a f) (mplusi (g a)
                 (lambdaf@ () (bindi (f) g)))))))

bind\ :sup:`i` (Scala)
----------------------

::

  def bind_i(a_inf: Stream[Subst], g: Goal): Stream[Subst] =
    a_inf match {
      case Stream.empty => a_inf
      case Stream.cons(a, f) => f match {
        case Stream.empty => g(a)
        case _ => mplus_i(g(a), bind(f, g))
      }
    }


The port: Debugging
-------------------

.. class:: incremental

- property specification allows for easy declaration of test cases
- can stress-test individual functions, and narrow down possible culprits
- stack overflow bug found in a combination of elimination and having comments

The port: Debugging (cont.)
---------------------------

When computing with streams, eagerness is *bad*

::

  $ git diff 5bc7a839ae9db cc596e43b465c
     /**
  -   * While we could use call-by-name here,
  -   * since the goals are functions anyway, delaying evaluation is
  -   * unnecessary
  ...
  -  def if_e(g0: Goal, g1: Goal, g2: Goal): Goal = {
  +  def if_e(testg: Goal, conseqg: Goal, altg: => Goal): Goal = {
  ...



The port: Downloads
-------------------

The Scala port is available under the BSD license from GitHub\ [#]_.
The latest Kanren source is available on Sourceforge\ [#]_.

.. [#] http://github.com/hircus/minikanren-scala
.. [#] http://kanren.sourceforge.net/

Porting the Mini-Kanren logic system to Scala
=============================================

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

1. The `Mini-Kanren`_ logic system
2. `Why Scala`_
3. `The port`_

Mini-Kanren
-----------

To many ears, the term *logic programming* is virtually synonymous
with Prolog (see [Colmerauer92]_ for a historical treatment). Outside
the domain of Artificial Intelligence, computer science practicioners
tend not to be exposed to the field -- in most cases, students are
first exposed to procedural, then object-oriented, then functional
languages\ [*]_.


.. [*] If they are (un)lucky, functional comes first
.. [Colmerauer92] *The birth of Prolog*, Colmerauer and Russell, 1992

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

Mini-Kanren: mplus
------------------

::

  (define mplus
    (lambda (a-inf f)
      (case-inf a-inf
        (f) 
        ((a) (choice a f))
        ((a f0) (choice a 
                  (lambdaf@ () (mplus (f0) f)))))))

Mini-Kanren: mplus\ :sup:`i`
----------------------------

::

  (define mplusi
    (lambda (a-inf f)
      (case-inf a-inf
        (f) 
        ((a) (choice a f))
        ((a f0) (choice a 
                  (lambdaf@ () (mplusi (f) f0)))))))


Mini-Kanren: bind
-----------------

::

  (define bind
    (lambda (a-inf g)
      (case-inf a-inf
        (mzero)
        ((a) (g a))
        ((a f) (mplus (g a)
                 (lambdaf@ () (bind (f) g)))))))

Mini-Kanren: bind\ :sup:`i`
---------------------------

::

  (define bindi
    (lambda (a-inf g)
      (case-inf a-inf
        (mzero)
        ((a) (g a))
        ((a f) (mplusi (g a)
                 (lambdaf@ () (bindi (f) g)))))))


Mini-Kanren: Conditionals
-------------------------

Two families of conditional constructs are provided -- *cond*-like
constructs and *if*-like constructs.

Mini-Kanren: Conditionals: if\ :sup:`aux`
-----------------------------------------

::

  (define-syntax ifaux
    (syntax-rules ()
      ((_ mplusfn g0 g1 g2)
       (lambdag@ (s)
         (mplusfn ((all g0 g1) s)
                  (lambdaf@ () (g2 s)))))))

Mini-Kanren: Conditionals: if\ :sup:`e` and if\ :sup:`i`
--------------------------------------------------------

::

  (define-syntax ife
    (syntax-rules ()
      ((_ g0 g1 g2)
       (ifaux mplus g0 g1 g2))))

  (define-syntax ifi
    (syntax-rules ()
      ((_ g0 g1 g2)
       (ifaux mplusi g0 g1 g2))))



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
      (if-e (null° l)
        #s
        (if-e (pair° l)
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

Why Scala
---------

Pros
~~~~

.. class:: incremental

- runs on the JVM
- interoperates well with Java
- and thus with other JVM languages
- provides functional programming constructs
- pattern-matching
- powerful type system


Why Scala (cont.)
-----------------

Cons
~~~~

.. class:: incremental

- no mutual TCO (blame Sun)
- No macros
- call-by-name provides same power (but not conciseness)

Brief Tour of Scala
-------------------

Objects
~~~~~~~


Objects serve two purposes:

.. class:: incremental

- as a code container (cf. Python modules)
- as singletons

.. class:: incremental

Let's look at a concrete example

Brief Tour of Scala
-------------------

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

Brief Tour of Scala
-------------------

REPL
~~~~

::

  $ scala
  scala> import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.MiniKanren._

  scala> val v = make_var('hello)
  scala> val v = make_var('hello)
  v: info.hircus.kanren.MiniKanren.Var = Var('hello,0)

  scala> val w = make_var('hello)
  w: info.hircus.kanren.MiniKanren.Var = Var('hello,1)

Brief Tour of Scala
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


The port: Downloads
-------------------

The Scala port is available under the BSD license from GitHub\ [#]_.
The latest Kanren source is available on Sourceforge\ [#]_.

.. [#] http://github.com/hircus/minikanren-scala
.. [#] http://kanren.sourceforge.net/

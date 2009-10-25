Porting the Mini-Kanren logic system to Scala
=============================================

:Author: Michel Alexandre Salim

.. image:: http://i.creativecommons.org/l/by-sa/3.0/us/88x31.png
   :height: 31px
   :width:  88px
   :alt:    Creative Commons License
   :align:  center

Navigation
----------

* Use arrow keys, PgUp/PgDn, and mouse clicks to navigate
* Press "**C**" for controls, and click the "|mode|" button to switch
  between presentation and handout/outline modes

.. |mode| unicode:: U+00D8 .. capital o with stroke


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

.. [Colmerauer92] *The birth of Prolog*, Colmerauer and Roussel, 1992
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

A *substitution* is a set of mappings from logical variables to values\
[#]_. It is immutable; extending a substitution with a new key-value
mapping produces a new substitution, with the old substitution remaining
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

- cond\ :sup:`e` -- each goal can succeed
- cond\ :sup:`i` -- each goal can succeed, output is interleaved
- cond\ :sup:`a` -- a single line, cf. soft-cut. only one goal can succeed
- cond\ :sup:`u` -- uni-. like cond\ :sup:`a`, but the successful
  *question* only succeeds once

.. class:: incremental

We'll stick with cond\ :sup:`e` first, and discuss the others in a bit

List predicate (Scheme)
-----------------------
::

  (def list?
    (λ (l)
      (cond
       ((null? l))
       ((pair? l)
        (list? (cdr l)))
       (else #f))))

A list is either an empty list, or a pair whose tail is a list


List predicate (Kanren)
-----------------------
    
::

  (def list°
    (λ (l)
      (conde
       ((null° l))
       ((pair° l)
        (fresh (d)
	  (cdr° l d)
          (list° d)))
       (else #u))))

List predicates
---------------

Note the differences:

- cond\ :sup:`e` instead of cond
- cdr\ :sup:`o` instead of cdr
- relations cannot be nested
- non-boolean relations take an extra argument
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


Scala: Tail-Call Optimization
-----------------------------

.. class:: incremental

- function calls in tail position should not grow call stack
- JVM does not have tailcall instruction
- JVM functional languages work around this to differing extents

Scala: TCO: self-recursion
--------------------------

This is safe:

::

  def even_or_odd(check_even: Boolean, n: Int) = n match {
    case 0 => check_even
    case _ => even_or_odd(!check_even, n-1)
  }

Scala: TCO: mutual recursion
----------------------------

This is not:

::

  def is_even(n: Int) = n match {
    case 0 => true
    case _ => is_odd(n-1)
  }

  def is_odd(n: Int) = n match {
    case 0 => false
    case _ => is_even(n-1)
  }

.. class:: incremental

- no mutual TCO (blame Sun)
- No macros
- call-by-name provides same power (but not conciseness)

Scala: Objects
--------------

Objects serve two purposes:

.. class:: incremental

- as a code container (cf. Python modules)
- in Java, this will be a class with static fields
- as singletons
- an object is automatically instantiated exactly once

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
  #

.. class:: incremental

::

  scala> def sum(l: List[Int]): Int = l match {
       | case Nil => 0
       | case h::tl => h + sum(tl)
       | }
  sum: (List[Int])Int

  scala>


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
    property("startsWith") = Prop.forAll((a: String, b: String) =>
      (a+b).startsWith(a))
    // Is this really always true?
    property("concat") = Prop.forAll((a: String, b: String) => 
      (a+b).length > a.length && (a+b).length > b.length )
    property("substring") = Prop.forAll((a: String, b: String) => 
      (a+b).substring(a.length) == b )
  }

The port
--------

The initial port was done over the course of several weeks; the
current implementation is a rewrite\ [#]_. The initial implementation
had a stack-overflow bug that was reëncountered during the rewrite,
which I'll discuss in a bit.

The new codebase is better tested, and utilizes more Scala features to
make the syntax look natural.

.. [#] original code is lost. moral story: backup (and share online...)

The port: Substitution
----------------------

Several choices for substitution:

.. class:: incremental

- List[(Var, Any)] --> equivalent to ((Var,Any),Subst)
- linked triples: (Var, Any, Subst)
- immutable maps

The port: Substitution (cont.)
------------------------------

Scheme Kanren uses *association lists*, i.e. a linked list of linked lists,
but that could be partly because that's the only native recursive data structure
in Scheme.

.. class:: incremental

- consider memory usage
- in Scala, triples are more than twice faster
- immutable maps ==> heap OOM


The port: Constraints
---------------------

Kanren does not natively understand numbers, so the most natural
constraint is inequality. (This is proposed by Prof. Friedman and is
not part of the official Kanren codebase, probably due to performance
cost)

This implementation led to the shift in the Scala port from an exact
translation of Scheme's substitution to a more OOP implementation
(cf. Haskell typeclass).

The port: Constraints (cont.)
-----------------------------

.. class:: incremental

- simple substitutions have no-op constraint methods
- constraint substitutions delegate to the simple substitution methods when
  possible, and layer constraint checking on top

The port: Constraints: code
---------------------------

:: 

  case class ConstraintSubstN(s: SimpleSubst,
                              c: Constraints) extends Subst {
    def extend(v: Var, x: Any) =
      if (this.constraints(v) contains x) None
      else Some(ConstraintSubstN(SimpleSubst(v,x,s), c))
  
    override def c_extend(v: Var, x: Any) =
      ConstraintSubstN(s, c_insert(v,x,c))

The port: Constraints: code
---------------------------

::

    def lookup(v: Var) = s.lookup(v)
    override def constraints(v: Var) = c_lookup(v, c)
    def length: Int = s.length 
  }


Monadic operator: mplus (Scheme)
--------------------------------

::

  (define mplus
    (lambda (a-inf f)
      (case-inf a-inf
        (f) 
        ((a) (choice a f))
        ((a f0) (choice a 
                  (lambdaf@ () (mplus (f0) f)))))))

Monadic operator: mplus (Scala)
-------------------------------

::

  def mplus(a_inf: Stream[Subst],
            f: => Stream[Subst]): Stream[Subst] =
    a_inf append f

.. class:: handout

**mplus** is simply stream append. It is kept as a separate function because,
as can be seen in the next slide, other variants do not have built-in Scala
implementations.

Monadic operator: mplus\ :sup:`i` (Scheme)
------------------------------------------

::

  (define mplusi
    (lambda (a-inf f)
      (case-inf a-inf
        (f) 
        ((a) (choice a f))
        ((a f0) (choice a 
                  (lambdaf@ () (mplusi (f) f0)))))))

**mplus**\ :sup:`i` *interleaves* two streams

Monadic operator: mplus\ :sup:`i` (Scala)
-----------------------------------------

::

  def mplus_i(a_inf: Stream[Subst],
            f: => Stream[Subst]): Stream[Subst] = a_inf match {
    case Stream.empty => f
    case Stream.cons(a, f0) => f0 match {
      case Stream.empty => Stream.cons(a, f)
      case _ => Stream.cons(a, mplus_i(f, f0))
    }
  }


Monadic operator: bind (Scheme)
-------------------------------

::

  (define bind
    (lambda (a-inf g)
      (case-inf a-inf
        (mzero)
        ((a) (g a))
        ((a f) (mplus (g a)
                 (lambdaf@ () (bind (f) g)))))))

Monadic operator: bind (Scala)
------------------------------

::

  def bind(a_inf: Stream[Subst], g: Goal): Stream[Subst] =
    a_inf flatMap g

.. class:: handout

**bind** is flatMap: it first maps *g* over the stream, and then append the
resulting streams together.

Monadic operator: bind\ :sup:`i` (Scheme)
-----------------------------------------

::

  (define bindi
    (lambda (a-inf g)
      (case-inf a-inf
        (mzero)
        ((a) (g a))
        ((a f) (mplusi (g a)
                 (lambdaf@ () (bindi (f) g)))))))

Monadic operator: bind\ :sup:`i` (Scala)
----------------------------------------

::

  def bind_i(a_inf: Stream[Subst], g: Goal): Stream[Subst] =
    a_inf match {
      case Stream.empty => a_inf
      case Stream.cons(a, f) => f match {
        case Stream.empty => g(a)
        case _ => mplus_i(g(a), bind(f, g))
      }
    }

Syntax: equality
----------------

In Scheme, (≡ x y) is the goal that unifies *x* and *y*; (≢ x y)
constrains them from being unifiable. The syntax looks natural in
Scheme, as everything is infix.

.. class:: incremental

In Scala, however, the equivalent looks ugly: *mkEqual(x,y)*;
*neverEqual(x,y)*. We can introduce infix operations by using implicit
conversions

Syntax: equality
----------------

::

  class Unifiable(a: Any) {
    def ===(b: Any): Goal = mkEqual(a, b)
    def =/=(b: Any): Goal = neverEqual(a, b)
  }

  implicit def unifiable(a: Any) = new Unifiable(a)

≡ and ≢ are now methods of the class *Unifiable*, and because an
implicit conversion function is in scope, attempting to call it on any
value will autobox it to a Unifiable with the same value.

The port: Macros
----------------

Most macros in the original code can be completely replaced by functions, apart
from the ones that introduce new names

The port: Macros: run
---------------------

::

  > (run #f (q) (member° q '(a b c d e)))
  (a b c d e)
  >

The port: Macros: run
---------------------

::

  (define-syntax run
    (syntax-rules ()
      ((_ n^ (x) g ...)
       (let ((n n^) (x (var x)))
         (if (or (not n) (> n 0))
	   (map-inf n
	     (lambda (s)
	       (reify (walk* x s)))
	     ((all g ...) empty-s))
	   ())))))

The port: Macros: Run
---------------------

::

    def run(n: Int, v: Var)(g0: Goal, gs: Goal*) = {
      val g = gs.toList match {
        case Nil => g0
	case gls => all((g0::gls): _*)
      }
      val allres = g(empty_s)  map {s: Subst => reify(walk_*(v, s)) }
      (if (n < 0) allres else (allres take n)) toList
    }

.. class:: handout

  - *v* must be already defined
  - We use the **map** method of a stream, which produces a lazy stream
  - It's not idiomatic outside Lisp to have functions that take either
    #f or some other type.  Instead, a negative number is used to
    collect all results

The port: Macros: fresh
-----------------------

::

  (def list°
    (λ (l)
      (conde
       ((null° l))
       ((pair° l)
        (fresh (d)
	  (cdr° l d)
          (list° d))))))

.. class:: incremental

This differs slightly from the first appearance of *list°*: the (else #u) line is removed,
as cond\ :sup:`e` fails by default

The port: Macros: fresh
-----------------------

::

  def list_o(l: Any): Goal = {
    cond_e((null_o(l), succeed),
           (pair_o(l), { s: Subst => {
                         val d = make_var('d)
                         both(cdr_o(l, d), list_o(d))(s) } }))
  }

.. class:: incremental

- unlike a macro, *cond_e* is evaluated at runtime.
- each line is required to have strictly 2 goals (thus **succeed** is inserted)
- the **fresh** goal is replaced by a closure. Note *s* is passed to **both**

The port: Macros: project
-------------------------

::

  >  (run 2 (x)
       (conde
        ((== x 7)  (project (x) (begin (printf "~s~n" x) succeed)))
        ((== x 42) (project (x) (begin (printf "~s~n" x) fail)))))
  7
  42
  (7)
  >

.. class:: handout

  - within the body of the projection, the logic variable *x* is
    replaced by its bound value
  - cond\ :sup:`e` successively bind *x* to 7 and 42
  - the second **project** expression fails after printing 42, thus 42
    is not in the result list


The port: Macros: project
-------------------------

::

  run(2, x)(cond_e((mkEqual(x,7), { s: Subst => {
                                    val x1 = walk_*(x, s)
                                    println(x1)
				    succeed(s) }}),
		   (mkEqual(x,42), { s: Subst => {
                                     val x1 = walk_*(x, s)
                                     println(x1)
				     fail(s) }})))
	    


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

The port: Common pitfalls
-------------------------

- when translating a Scheme **fresh** or **project** goal, forgetting
  to apply the created goal to the input substitution
- higher-order functions: functional parameter must be followed by *_*
- Variadic functions: if arg array is converted internally to arg list,
  must convert back to arg array when recurring


The port: Benchmarks: Petite Chez Scheme
----------------------------------------

::

  > (time (run 1 (q) (palprod2 q)))
  100001
  101101
  (time (run 1 ...))
      315 collections
      37916 ms elapsed cpu time, including 156 ms collecting
      38858 ms elapsed real time, including 161 ms collecting
      1330081488 bytes allocated, including 1325728560 bytes reclaimed
  ((1 1 1 0 0 1 1 1 1 1 0 0 0 1))


The port: Benchmarks: Scala (association list)
----------------------------------------------

::

  scala> time(run(1,x)(palprod_o(x)))
  100001
  101101
  Elapsed: 114344 ms
  res2: Any = List((1,(1,(1,(0,(0,(1,(1,(1,(1,(1,(0,(0,(0,(1,List()...

The port: Benchmarks: Scala (case class)
----------------------------------------

::

  scala> time(run(1,x)(palprod_o(x)))
  100001
  101101
  Elapsed: 44277 ms
  res2: Any = List((1,(1,(1,(0,(0,(1,(1,(1,(1,(1,(0,(0,(0,(1,List()...



The port: TODO list
-------------------

.. class:: incremental

- parallelization: cf. pmap\ [#]_
- the problem is that we don't want to precompute too many answers, so
  unlike a list pmap, a stream pmap will have to precompute only a
  fixed number of elements
- Prolog benchmarks from the full Kanren

.. [#] Erlang implementation: http://lukego.livejournal.com/6753.html


The port: Downloads
-------------------

The Scala port is available under the BSD license from GitHub\ [#]_.
The latest Kanren source is available on Sourceforge\ [#]_.

.. [#] http://github.com/hircus/minikanren-scala
.. [#] http://kanren.sourceforge.net/

Q&A
---

Your questions, suggestions, etc. are welcome! The project bug tracker is
at the GitHub address.

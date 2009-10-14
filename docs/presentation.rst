Porting the Mini-Kanren logic system to Scala
=============================================

Abstract
--------

Mini-Kanren is a simplified implementation of Kanren, a declarative
logic system, embedded in a pure functional subset of Scheme.

It is used at Indiana University in both undergraduate and graduate
programming language courses, with graduate students being required to
reimplement MK in a (non-Lisp) programming language of their choice.


Outline
-------

This presentation is in three sections:

1. The `Mini-Kanren`_ logic system
2. `Why Scala`_
3. `The port`_

Mini-Kanren
-----------


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

The port
--------



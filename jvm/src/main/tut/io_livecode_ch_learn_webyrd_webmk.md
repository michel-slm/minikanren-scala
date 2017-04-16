# miniKanren: an interactive Tutorial


## Core miniKanren

Core miniKanren extends Scheme with three operations:
`==` (here in scala: `unify`/`mkEqual`/`===`), `fresh` (here in scala: `make_var`), and `conde`.
There is also `run`, which serves as an interface between
Scheme and miniKanren, and whose value is a list.

`==` unifies two terms. `fresh`, which
syntactically looks like `lambda`, introduces lexically-scoped
Scheme variables that are bound to new logic variables; `fresh`
also performs conjunction of the relations within its body. Thus
`(fresh (x y z) (== x z) (== 3 y))`

```tut:silent
import info.hircus.kanren._
import info.hircus.kanren.MiniKanren._
import info.hircus.kanren.MKMath._
val x = make_var('x)
val y = make_var('y)
val z = make_var('z)
all(mkEqual(x, z), mkEqual(3, y))
```

would introduce logic variables `x`, `y`, and `z`,
then associate `x` with `z` and `y`
with `3`.  This, however, is not a legal miniKanren
program---we must wrap a `run` around the entire expression.
`(run 1 (q) (fresh (x y z) (== x z) (== 3 y)))`

   ```tut
val q = make_var('q)
run(1, q)(all(mkEqual(make_var('x), make_var('z)), mkEqual(build_num(3), make_var('y))))
   ```

The value returned is a list containing the single
value `(_.0)`; we
say that `_.0` is
the *reified value* of the unbound query variable `q` and thus
represents any value. `q` also remains unbound in
`(run 1 (q) (fresh (x y) (== x q) (== 3 y)))`

   ```tut
run(1, q)(all(mkEqual(make_var('x), q), mkEqual(build_num(3), make_var('y))))
   ```

We can get back more interesting values by unifying the query variable with another term.

    (run 1 (y)
      (fresh (x z)
        (== x z)
        (== 3 y)))

```tut
val ex3_x = make_var('x)
val ex3_z = make_var('z)
val three = build_num(3)
run(1, y)(all(mkEqual(ex3_x, ex3_z), mkEqual(build_num(3), y)))
```
> Note that numbers are represented in binary form, so `3` (`three`) is `11` (`(1, (1, List()))`, similar to `HList`s, these are encoded as tuples).

    (run 1 (q)
      (fresh (x z)
        (== x z)
        (== 3 z)
        (== q x)))

```tut
val ex4_x = make_var('x)
val ex4_z = make_var('z)
run(1, q)(all(mkEqual(ex4_x, ex4_z), mkEqual(build_num(3), ex4_z), mkEqual(q, ex4_x)))
```

    (run 1 (y)
      (fresh (x y)
        (== 4 x)
        (== x y))
      (== 3 y))

```tut
val ex5_x = make_var('x)
run(1, y)(all(mkEqual(build_num(4), ex5_x), mkEqual(ex5_x, make_var('y)), mkEqual(build_num(3), y)))
```

Each of these examples returns `(3)`; in the
rightmost example, the `y` introduced by `fresh` is
different from the `y` introduced by `run`.

<!---<p>A <code>run</code> expression can return the empty list, indicating that
the body of the expression is logically inconsistent.</p>

<div class="live" id="ex7">
(run 1 (x) (== 4 3))</div>
<div class="live" id="ex8">
(run 1 (x) (== 5 x) (== 6 x))</div>

<p>We say that a logically inconsistent relation <em>fails</em>,
while a logically consistent relation, such as <code>(== 3 3)</code>, <em>succeeds</em>.</p>

<p><code>conde</code>, which resembles <code>cond</code> syntactically, is used
to produce multiple answers.  Logically, <code>conde</code> can be thought
of as disjunctive normal form: each clause represents a disjunct, and
is independent of the other clauses, with the relations within a
clause acting as the conjuncts. For example, this expression produces
two answers.</p>

<div class="live" id="ex9">
(run 2 (q)
  (fresh (w x y)
    (conde
      ((== `(,x ,w ,x) q)
       (== y w))
      ((== `(,w ,x ,w) q)
       (== y w)))))</div>

<p>Although the two <code>conde</code> lines are different, the
values returned are identical.  This is because distinct reified
unbound variables are assigned distinct subscripts, increasing from
left to right&mdash;the numbering starts over again from zero within each
answer, which is why the reified value of <code>x</code>
is <code>_.0</code> in the
first answer
but <code>_.1</code> in the
second.  The argument <code>2</code> in <code>run</code> denotes the
maximum length of the resultant list.  If <code>run*</code>
is used instead, then there is no maximum imposed.  This can easily lead to
infinite loops.
</p>

<div class="live" id="ex10">
(run* (q)
  (let loop ()
    (conde
      ((== #f q))
      ((== #t q))
      ((loop)))))</div>

<p>If we replace <code>*</code> by a natural number <code>n</code>,
then an <code>n</code>-element list of alternating <code>#f</code>'s
and <code>#t</code>'s is returned.
The first answer is produced by the first <code>conde</code> clause, which associates <code>q</code> with <code>#f</code>.
To produce the second answer, the second <code>conde</code>
clause is tried. Since <code>conde</code> clauses are independent, the association between <code>q</code> and
<code>#f</code> made in the first clause is forgotten---we say that <code>q</code> has been
<em>refreshed</em>.  In the third <code>conde</code> clause, <code>q</code> is
refreshed again.</p>

We now look at several interesting examples that rely on <code>anyo</code>,
which tries <code>g</code> an unbounded number of times.

<div class="live norun" id="anyo">
(define anyo
  (lambda (g)
    (conde
      (g)
      ((anyo g)))))</div>

<p>Consider the first example,</p>
<div class="live" id="ex11" data-lib="anyo">
(run* (q)
  (conde
    ((anyo (== #f q)))
    ((== #t q))))</div>

<p>which does not terminate because the call to <code>anyo</code>
succeeds an unbounded number of times.  If <code>*</code> were replaced by
<code>5</code>, then we would get <code>(#t #f #f #f #f)</code>.
(The user should not be concerned with the order of the answers produced.)</p>

<p>Now consider</p>

<div class="live" id="ex12" data-lib="anyo">
(run 10 (q)
  (anyo
    (conde
      ((== 1 q))
      ((== 2 q))
      ((== 3 q)))))</div>

<p>Here the values <code>1</code>, <code>2</code>, and <code>3</code> are
interleaved; our use of <code>anyo</code> ensures that this sequence is
repeated indefinitely.
</p>

<p>Even if a relation within a <code>conde</code> clause loops
indefinitely (or <em>diverges</em>), other <code>conde</code> clauses can
contribute to the answers returned by a <code>run</code> expression.  For
example,</p>

<div class="live" id="ex13" data-lib="anyo">
(run 3 (q)
  (let ((nevero (anyo (== #f #t))))
    (conde
      ((== 1 q))
      (nevero)
      ((conde
         ((== 2 q))
         (nevero)
         ((== 3 q)))))))</div>

<p>returns <code>(1 2 3)</code>. Replacing
<code>run 3</code> with <code>run 4</code> would cause divergence, since <code>nevero</code> would loop
indefinitely looking for the non-existent fourth answer.</p>

<h2>Additional Constraint Operators</h2>

<p>We extend core miniKanren with four constraint operators: the
disequality constraint <code>=/=</code> (previously described in the
context of the cKanren constraint logic programming framework
cKanren); type constraints <code>symbolo</code> and
<code>numbero</code>, which are the miniKanren equivalent
of Scheme's <code>symbol?</code> and <code>number?</code> type predicates; and
<code>absento</code>}, which ensures a symbol <code>tag</code> does not occur
in a term <code>t</code>.
</p>

<div class="live" id="ex14">
(run* (q) (symbolo q))</div>

<p>
The single answer  <code>(_.0 (sym _.0))</code>
indicates that <code>q</code> remains unbound, and also that <code>q</code> represents a symbol. Any attempt to associate
<code>q</code> with a non-symbol value should therefore lead to failure.
</p>

<div class="live" id="ex15">
(run* (q)
  (symbolo q)
  (== 4 q))</div>
<div class="live" id="ex16">
(run* (q)
  (symbolo q)
  (numbero q))</div>

<p>Try replacing all occurrences of <code>symbolo</code> by <code>numbero</code> in the
three examples above.</p>

<p>Next we consider the disequality constraint <code>=/=</code>.</p>

<div class="live" id="ex17">
(run* (p) (=/= p 1))</div>

<p>The answer states that <code>p</code> remains unbound, but cannot be associated with
<code>1</code>.  Of course, violating the constraint leads to failure:</p>

<div class="live" id="ex18">
(run* (p) (=/= 1 p) (== 1 p))</div>

<p>A slightly more complicated example is a disequality constraint between two lists.</p>

<div class="live" id="ex19">
(run* (q)
  (fresh (p r)
    (=/= '(1 2) `(,p ,r))
    (== `(,p ,r) q)))</div>

<p>The answer states that <code>p</code> and <code>r</code> are
unbound, and that <code>p</code> cannot be associated with <code>1</code>
while <code>r</code> is associated with <code>2</code>.</p>

<p>
We would get the same answer if we were to replace
<code>(=/= '(1 2) `(,p ,r))</code> by either
<code>(=/= '((1) (2)) `((,p) (,r)))</code>
or
<code>(=/= `((1) (,r)) `((,p) (2)))</code>.
</p>

<p>Now consider the <code>run</code> expression</p>

<div class="live" id="ex20">
(run* (q)
  (fresh (p r)
    (=/= '(1 2) `(,p ,r))
    (== 1 p)
    (== `(,p ,r) q)))</div>

<p>If we also associate <code>r</code> with <code>2</code>, the <code>run</code> expression fails.</p>

<div class="live" id="ex21">
(run* (q)
  (fresh (p r)
    (=/= '(1 2) `(,p ,r))
    (== 1 p)
    (== 2 r)
    (== `(,p ,r) q)))</div>

<p> Now consider what happens when <code>(== 2 r)</code> is replaced
 by <code>(symbolo r)</code> in the previous example. Then the
 <code>run</code> expression succeeds with an answer
 which states that <code>r</code> can only be associated with a symbol.
 The reified constraint  <code>(=/= ((_.0 2)))</code> (stating that <code>r</code> cannot be associated with <code>2</code>) is not
 included in the answer, since it is subsumed by the constraint that
 <code>r</code> must be a symbol.
</p>

<p> Finally we consider <code>absento</code>, which ensures a symbol <code>tag</code> does not appear in a term <code>t</code>.
 Assume we have a term <code>q</code> containing predators such as <code>jackal</code>s  and <code>leopard</code>s,
 and we desire to keep gentle <code>panda</code>s out of this dangerous term.  We can use <code>absento</code> to ensure that this will occur.</p>

<div class="live" id="ex22">
(run* (q)
  (fresh (x y)
    (== `(jackal (,y leopard ,x)) q)
    (absento 'panda q)))</div>

<p>
The answer states that the two unbound variables, <code>x</code>
and <code>y</code>, cannot be associated with a term that contains the
term <code>panda</code>.  If we violate this constraint by associating <code>x</code> with
<code>panda</code> (or with a list containing <code>panda</code>), the <code>run</code> expression
no longer returns any answers, keeping the <code>panda</code>s safe.
</p>

<div class="live" id="ex23">
(run* (q)
  (fresh (x y)
    (== `(jackal (,y leopard ,x)) q)
    (absento 'panda q)
    (== 'panda x)))</div>

<p>
If <code>x</code> is known to be a symbol, the <code>absento</code>
constraint on <code>x</code> can be simplified to a disequality constraint
between <code>x</code> and <code>panda</code>.</p>

<div class="live" id="ex24">
(run* (q)
  (fresh (x y)
    (== `(jackal (,y leopard ,x)) q)
    (absento 'panda q)
    (symbolo x)))</div>

<p>The answer still contains the full <code>absento</code> constraint on
<code>y</code>; violating this constraint does indeed cause failure.</p>

<div class="live" id="ex25">
(run* (q)
  (fresh (x y z)
    (== `(jackal (,y leopard ,x)) q)
    (absento 'panda q)
    (symbolo x)
    (== `(c ,z d) y)
    (== 'panda z)))</div>

-->
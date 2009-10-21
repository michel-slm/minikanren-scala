/*
 * Copyright (c) 2009 Michel Alexandre Salim.  All rights reserved.
 *
 * Redistribution and use in source and binary forms, with or without
 * modification, are permitted provided that the following conditions
 * are met:
 *
 * 1. Redistributions of source code must retain the above copyright
 *    notice, this list of conditions and the following disclaimer.
 * 2. Redistributions in binary form must reproduce the above copyright
 *    notice, this list of conditions and the following disclaimer in
 *    the documentation and/or other materials provided with the
 *    distribution.
 * 3. The names of the authors may not be used to endorse or promote
 *    products derived from this software without specific, prior
 *    written permission.
 *
 * THIS SOFTWARE IS PROVIDED BY THE COPYRIGHT HOLDERS AND CONTRIBUTORS
 * ``AS IS'' AND ANY EXPRESS OR IMPLIED WARRANTIES, INCLUDING, BUT NOT
 * LIMITED TO, THE IMPLIED WARRANTIES OF MERCHANTABILITY AND FITNESS
 * FOR A PARTICULAR PURPOSE ARE DISCLAIMED.  IN NO EVENT SHALL THE
 * COPYRIGHT HOLDERS OR CONTRIBUTORS BE LIABLE FOR ANY DIRECT, INDIRECT,
 * INCIDENTAL, SPECIAL, EXEMPLARY OR CONSEQUENTIAL DAMAGES (INCLUDING,
 * BUT NOT LIMITED TO, PROCUREMENT OF SUBSTITUTE GOODS OR SERVICES;
 * LOSS OF USE, DATA, OR PROFITS; OR BUSINESS INTERRUPTION) HOWEVER CAUSED
 * AND ON ANY THEORY OF LIABILITY, WHETHER IN CONTRACT, STRICT LIABILITY,
 * OR TORT (INCLUDING NEGLIGENCE OR OTHERWISE) ARISING IN ANY WAY OUT
 * OF THE USE OF THIS SOFTWARE, EVEN IF ADVISED OF THE POSSIBILITY OF
 * SUCH DAMAGE.
 */

package info.hircus.kanren

object MiniKanren {

  /* Type definitions */
  import java.util.HashMap

  /**
   * A constraint is a list of pairs, each pair consisting of a logical variable and a list of
   * variables/values it is not allowed to unify with
   */
  type Constraints = List[(Var, List[Any])]

  /**
   * This abstract class specifies the basic operations any substitution must satisfy.
   */
  abstract class Subst {
    /**
     * Extend a substitution with a new mapping from v -> x. Might fail in some substitution implementations.
     */
    def extend(v: Var, x: Any): Option[Subst]
    /**
     * Add a constraint for the specified variable
     */
    def c_extend(v: Var, x: Any): Subst = this
    /**
     * Given a variable, look up its constraints
     */
    def constraints(v: Var): List[Any] = Nil
    /**
     * Given a variable, look up its bound value
     */
    def lookup(v: Var): Option[Any]
    /**
     * The length of a substitution, i.e. the number of var -> value mappings it contains
     */
    def length: Int

    /**
     * Unifies two terms
     * This default implementation always succeeds; substitution classes with constraints
     * must override this, but may call this implementation once the unification is verified to be safe
     *
     * @param term1 Any value
     * @param term2 Any value
     * @return Some substitution
     */
    def unify(term1: Any, term2: Any): Option[Subst] = {
      val t1 = walk(term1, this)
      val t2 = walk(term2, this)

      if (t1 == t2) return Some(this)
      else if (t1.isInstanceOf[Var])
	return this.extend(t1.asInstanceOf[Var], t2)
      else if (t2.isInstanceOf[Var])
	return this.extend(t2.asInstanceOf[Var], t1)
      else if (pairp(t1) && pairp(t2)) {
	val ls1 = t1.asInstanceOf[(Any,Any)]
	val ls2 = t2.asInstanceOf[(Any,Any)]

	this.unify(ls1._1, ls2._1) match {
	  case None => return None
	  case Some(s2: Subst) =>
	    return s2.unify(ls1._2, ls2._2)
	}
      }
      else if (t1 == t2) return Some(this)
      else return None
    }
  }

  import info.hircus.kanren.Substitution._

  /**
   * A goal is a function that, given a substitution, produces a stream of substitution.
   * This stream is empty if the goal fails; otherwise, it may contain any number of
   * substitutions
   */
  type Goal = (Subst) => Stream[Subst]
  val empty_s  = EmptySubst
  val empty_cs = ConstraintSubst0(Nil)

  /**
   * A logic variable
   * It consists of two parts: a user-supplied name, and a count that is automatically incremented.
   * The count makes sure that each created variable is unique.
   */
  case class Var(name: Symbol, count: Int)
  private val m = new HashMap[Symbol, Int]()
  /**
   * Creates a logic variable, with the requested name, and a count that is automatically incremented
   *
   * @param name The name of the variable
   * @return a logic variable
   */
  def make_var(name: Symbol) = {
    val count = m.get(name)
    m.put(name, count+1)
    Var(name, count)
  }

  /* Monads */

  /**
   * A goal that always succeeds, returning a stream containing only its input substitution
   */
  def succeed: Goal = { s: Subst =>
    Stream.cons(s, Stream.empty)
  }
  /**
   * A goal that always fails, returning an empty stream of substitution
   */
  def fail: Goal = { s: Subst => Stream.empty }


  def pairp(x: Any): Boolean =
    x.isInstanceOf[(Any,Any)]

/*
 * (define walk
 *   (lambda (v s)
 *     (cond
 *       ((var? v)
 *        (cond
 *          ((assq v s) =>
 *           (lambda (a)
 *             (let ((v^ (rhs a)))
 *               (walk v^ s))))
 *          (else v)))
 *       (else v))))
 *
 * 
*/

  def walk(v: Any, s: Subst): Any =
    if (v.isInstanceOf[Var]) s.lookup(v.asInstanceOf[Var]) match {
      case Some(x) => walk(x, s)
      case None => v
    } else v

/*
 * (define walk*
 *   (lambda (v s)
 *     (let ((v (walk v s)))
 *       (cond
 *         ((var? v) v)
 *         ((pair? v)
 *          (cons
 *            (walk* (car v) s)
 *            (walk* (cdr v) s)))
 *         (else v)))))
*/
  def walk_*(v: Any, s: Subst): Any = {
    val v1 = walk(v, s)
    if (v1.isInstanceOf[Var]) v1
    else if (pairp(v1)) {
      val ls = v1.asInstanceOf[(Any,Any)]
      (walk_*(ls._1, s), walk_*(ls._2, s))
    } else v1
  }

/* (define reify-s
 *   (lambda (v s)
 *     (let ((v (walk v s)))
 *       (cond
 *         ((var? v) (ext-s v (reify-name (size-s s)) s))
 *         ((pair? v) (reify-s (cdr v) (reify-s (car v) s)))
 *         (else s)))))
 *
 * (define reify-name
 *   (lambda (n)
 *     (string->symbol
 *       (string-append "_" "." (number->string n)))))
 */

  def reify_name(n: Int) =
    Symbol("_." + n)
  
  def reify_s(v: Any, s: Subst): Subst= {
    val v1 = walk(v, s)
    if (v1.isInstanceOf[Var])
      s.extend(v1.asInstanceOf[Var], reify_name(s.length)) match {
	case Some(s1) => s1
	/* never happens as reification does not use any constraints
	 * but the compiler does not know that
	 */
	case _ => s
      }
    else if (pairp(v1)) {
      val ls = v1.asInstanceOf[(Any,Any)]
      reify_s(ls._2, reify_s(ls._1, s))
    } else s
  }

/* (define reify
 *   (lambda (v)
 *     (walk* v (reify-s v empty-s))))
 */    
  def reify(v: Any) = walk_*(v, reify_s(v, empty_s))

  /* Logic system */

  /* (define bind
   *   (lambda (a-inf g)
   *     (case-inf a-inf
   *       (mzero)
   *       ((a) (g a))
   *       ((a f) (mplus (g a)
   *                (lambdaf@ () (bind (f) g)))))))
   */
  def bind(a_inf: Stream[Subst], g: Goal): Stream[Subst] =
    a_inf flatMap g

  def bind_i(a_inf: Stream[Subst], g: Goal): Stream[Subst] =
    a_inf match {
      case Stream.empty => a_inf
      case Stream.cons(a, f) => f match {
	case Stream.empty => g(a)
	case _ => mplus_i(g(a), bind(f, g))
      }
    }

  /* (define mplus
   *   (lambda (a-inf f)
   *     (case-inf a-inf
   *       (f)
   *       ((a) (choice a f))
   *       ((a f0) (choice a
   *                 (lambdaf@ () (mplus (f0) f)))))))
   */
  def mplus(a_inf: Stream[Subst],
	    f: => Stream[Subst]): Stream[Subst] =
    a_inf append f

  /**
   * Like mplus, but interleaves the two input streams
   * Allows a goal to proceed even if the first subgoal is bottom
   *
   * @param a_inf a stream of substitutions
   * @param f     a second stream of substitutions to append
   * @return an interleaved stream of substitutions
   */
  def mplus_i(a_inf: Stream[Subst],
	    f: => Stream[Subst]): Stream[Subst] = a_inf match {
    case Stream.empty => f
    case Stream.cons(a, f0) => f0 match {
      case Stream.empty => Stream.cons(a, f)
      case _ => Stream.cons(a, mplus_i(f, f0))
    }

  }


  /* (define-syntax anye
    *   (syntax-rules ()
    *     ((_ g1 g2)
    *      (lambdag@ (s)
    *        (mplus (g1 s)
    *          (lambdaf@ () (g2 s)))))))
    */
  def any_e(g1: Goal, g2: Goal): Goal = { s: Subst =>
    mplus(g1(s), g2(s)) }

  /* (define-syntax all
   *   (syntax-rules ()
   *     ((_) succeed)
   *     ((_ g) (lambdag@ (s) (g s)))
   *     ((_ g^ g ...) (lambdag@ (s) (bind (g^ s) (all g ...))))))
   */
  def all_aux(bindfn: (Stream[Subst], Goal) => Stream[Subst])(gs: Goal*): Goal = {
    gs.toList match {
      case Nil => succeed
      case g :: Nil => g
      case g :: gs2 =>
	{ s: Subst => bindfn(g(s), all(gs2: _*)) }
    }
  }

  def all   = all_aux(bind) _
  def all_i = all_aux(bind_i) _


  /**
   * Faster than all, if only two goals are used
   */
  def both(g0: Goal, g1: Goal): Goal = { s: Subst =>
    g0(s) flatMap g1 }

  /* (define-syntax ife
   *   (syntax-rules ()
   *     ((_ g0 g1 g2)
   *      (lambdag@ (s)
   *        (mplus ((all g0 g1) s)
   *               (lambdaf@ () (g2 s)))))))
   */

  /**
   * if_e produces a goal that, given a substitution, produces a stream of substitutions
   * starting with the result of running a combination of the first two goals on the substitution,
   * followed by running the alternate goal.
   *
   * @param testg   The first, 'test' goal. Guards the consequent
   * @param conseqg The 'consequent' goal
   * @param altg    The alternate goal. Call-by-name as otherwise, in a situation with many nested if_e
   *   (e.g. using any_o), the stack overflows.
   */
  def if_e(testg: Goal, conseqg: =>Goal, altg: =>Goal): Goal = {
    s: Subst =>
      mplus(both(testg, conseqg)(s),
	    altg(s))
  }

  def if_i(testg: Goal, conseqg: =>Goal, altg: =>Goal): Goal = {
    s: Subst =>
      mplus_i(both(testg, conseqg)(s),
	    altg(s))
  }

  def if_a(testg: Goal, conseqg: =>Goal, altg: =>Goal): Goal = {
    s: Subst => {
      val s_inf = testg(s)
      s_inf match {
	case Stream.empty => altg(s)
	case Stream.cons(s_1, s_inf_1) => s_inf_1 match {
	  case Stream.empty => conseqg(s_1)
	  case _ => bind(s_inf, conseqg) } }
    } }

  def if_u(testg: Goal, conseqg: =>Goal, altg: =>Goal): Goal = {
    s: Subst => {
      testg(s) match {
	case Stream.empty => altg(s)
	case Stream.cons(s_1, s_inf) => conseqg(s_1) }
    } }

  def cond_aux(ifer: (Goal, =>Goal, =>Goal) => Goal)(gs: (Goal,Goal)*): Goal =
    { gs.toList match {
      case Nil => fail
      case (g0, g1) :: gs2 => gs2 match {
	case Nil => both(g0, g1)
	case _ => ifer(g0, g1,
		       cond_aux(ifer)(gs2: _*))
      } } }

  def cond_e = cond_aux(if_e _) _
  def cond_i = cond_aux(if_i _) _
  def cond_a = cond_aux(if_a _) _
  def cond_u = cond_aux(if_u _) _

  class Unifiable(a: Any) {
    def ===(b: Any): Goal = mkEqual(a, b)
    def =/=(b: Any): Goal = neverEqual(a, b)
  }

  implicit def unifiable(a: Any) = new Unifiable(a)

  def mkEqual(t1: Any, t2: Any): Goal = { s: Subst => {
    s.unify(t1, t2) match {
      case Some(s2) => succeed(s2)
      case None => fail(s) // does not matter which substitution
    }
  } }

  def neverEqual(t1: Any, t2: Any): Goal = { s: Subst => {
    val v1 = walk(t1, s)
    val v2 = walk(t2, s)

    if (v1 == v2) fail(s)
    else {
      val s1 = if (v1.isInstanceOf[Var])  s.c_extend(v1.asInstanceOf[Var], v2) else s
      val s2 = if (v2.isInstanceOf[Var]) s1.c_extend(v2.asInstanceOf[Var], v1) else s1
      
      succeed(s2)
    }
  } }

  /* (define-syntax run
   *   (syntax-rules ()
   *     ((_ n^ (x) g ...)
   *      (let ((n n^) (x (var 'x)))
   *        (if (or (not n) (> n 0))
   *          (map-inf n
   *            (lambda (s) (reify (walk* x s)))
   *            ((all g ...) empty-s))
   *          '())))))
   */

  /**
   * Runs the given goals and produce up to n results for the specified variable
   *
   * @param n  max number of results. A negative number specifies that all available results should be returned
   * @param v  the variable to be inspected
   * @param g0 a goal; multiple goals might be specified
   */
  def run(n: Int, v: Var) = run_aux(n, v, false) _
  def crun(n: Int, v: Var) = run_aux(n, v, true) _
 
  private def run_aux(n: Int, v: Var, use_constraints: Boolean)(g0: Goal, gs: Goal*): List[Any] = {
    val g = gs.toList match {
      case Nil => g0
      case gls => all((g0::gls): _*)
    }
    val allres = g(if (use_constraints) empty_cs else empty_s)  map {s: Subst => reify(walk_*(v, s)) }
    (if (n < 0) allres else (allres take n)) toList
  }
}

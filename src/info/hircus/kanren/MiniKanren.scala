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
  import java.util.HashMap

  /* Monads */
  def succeed: Goal = { s: Subst =>
    Stream.cons(s, Stream.empty)
  }
  def fail: Goal = { s: Subst => Stream.empty }

  /* Logic variables */
  case class Var(name: Symbol, count: Int)
  private val m = new HashMap[Symbol, Int]()
  def make_var(name: Symbol) = {
    val count = m.get(name)
    m.put(name, count+1)
    Var(name, count)
  }

  type Binding = (Var, Any)

  /* Substitution */
  type Subst = List[Binding]
  val empty_s = Nil

  def ext_s(v: Var, x: Any, s: Subst): Subst = (v, x) :: s

  def lookup(v: Any, s: List[(Any,Any)]): Option[Any] =
    s match {
      case Nil => None
      case (v1, x: Any) :: s2 => if (v==v1) Some(x) else lookup(v, s2)
    }

  type Goal = (Subst) => Stream[Subst]
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
    if (v.isInstanceOf[Var]) lookup(v, s) match {
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
    if (v1.isInstanceOf[Var]) ext_s(v1.asInstanceOf[Var], reify_name(s.length), s)
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

  def unify(term1: Any, term2: Any, s: Subst): Option[Subst] = {
    val t1 = walk(term1, s)
    val t2 = walk(term2, s)

    if (t1 == t2) return Some(s)
    else if (t1.isInstanceOf[Var])
      return Some(ext_s(t1.asInstanceOf[Var], t2, s))
    else if (t2.isInstanceOf[Var])
      return Some(ext_s(t2.asInstanceOf[Var], t1, s))
    else if (pairp(t1) && pairp(t2)) {
      val ls1 = t1.asInstanceOf[(Any,Any)]
      val ls2 = t2.asInstanceOf[(Any,Any)]

      unify(ls1._1, ls2._1, s) match {
	case None => return None
	case Some(s2: Subst) =>
	  return unify(ls1._2, ls2._2, s2)
      }
    }
    else if (t1 == t2) return Some(s)
    else return None
  }
  
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
  def all(gs: Goal*): Goal = {
    gs.toList match {
      case Nil => succeed
      case g :: Nil => g
      case g :: gs2 =>
	{ s: Subst => bind(g(s), all(gs2: _*)) }
    }
  }

  /* (define-syntax ife
   *   (syntax-rules ()
   *     ((_ g0 g1 g2)
   *      (lambdag@ (s)
   *        (mplus ((all g0 g1) s)
   *               (lambdaf@ () (g2 s)))))))
   */

  /**
   * While we could use call-by-name here,
   * since the goals are functions anyway, delaying evaluation is
   * unnecessary
   */
  def if_e(g0: Goal, g1: Goal, g2: Goal): Goal = {
    s: Subst =>
      mplus(all(g0, g1)(s),
	    g2(s))
  }

  def mkEqual(t1: Any, t2: Any): Goal = { s: Subst =>
    unify(t1, t2, s) match {
      case Some(s2) => succeed(s2)
      case None => fail(empty_s) // does not matter which substitution
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

  /* produce at most n results
   */
  def run(n: Int, v: Var)(g: Goal) = {
    val allres = g(empty_s)  map {s: Subst => reify(walk_*(v, s)) }
    (if (n < 0) allres else (allres take n)) toList
  }
}

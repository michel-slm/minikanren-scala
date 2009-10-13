package info.hircus.kanren

object MKLogic {
  import info.hircus.kanren.MiniKanren._
  
  /* Logic system */

/* (define bind
 *   (lambda (a-inf g)
 *     (case-inf a-inf
 *       (mzero)
 *       ((a) (g a))
 *       ((a f) (mplus (g a)
 *                (lambdaf@ () (bind (f) g)))))))
 */
  def bind(a_inf: Stream[Subst], g: Goal): Stream[Subst] = {
    a_inf match {
      case Stream.empty => a_inf
      case Stream.cons(a: Subst, f: Stream[Subst]) =>
	f match {
	  case Stream.empty => g(a)
	  case _ =>
	    mplus(g(a), bind(f, g))
	}
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


/* (define-syntax anye
  *   (syntax-rules ()
  *     ((_ g1 g2)
  *      (lambdag@ (s)
  *        (mplus (g1 s)
  *          (lambdaf@ () (g2 s)))))))
  */
  def any_e(g1: Goal, g2: Goal)(s: Subst) = {
    mplus(g1(s), g2(s))
  }

/* (define-syntax all
 *   (syntax-rules ()
 *     ((_) succeed)
 *     ((_ g) (lambdag@ (s) (g s)))
 *     ((_ g^ g ...) (lambdag@ (s) (bind (g^ s) (all g ...))))))
 */

  def all(gs: Goal*): Goal = {
    gs.toList match {
      case Nil => succeed _
      case g :: Nil => g
      case g :: gs2 =>
	{ s: Subst => bind(g(s), all(gs2: _*)) }
    }
  }
/* (define-syntax conde
 *   (syntax-rules (else)
 *     ((_) fail)
 *     ((_ (else g0 g ...)) (all g0 g ...))
 *     ((_ (g0 g ...) c ...)
 *      (anye (all g0 g ...) (conde c ...)))))
 */

  def cond_e(cs: List[List[Goal]]): Goal = {
    cs match {
      case Nil => fail _
      case goals :: more_cs =>
	any_e(all(goals: _*), cond_e(more_cs))
    }
  }

/* (define-syntax case-inf
 *   (syntax-rules ()
 *     ((_ e on-zero ((a^) on-one) ((a f) on-choice))
 *      (let ((a-inf e))
 *        (cond
 *          ((not a-inf) on-zero)
 *          ((not (and
 *                  (pair? a-inf)
 *                  (procedure? (cdr a-inf))))
 *           (let ((a^ a-inf))
 *             on-one))
 *          (else (let ((a (car a-inf))
 *                      (f (cdr a-inf)))
 *                  on-choice)))))))
 * 
 */
  def mkEqual(t1: Any, t2: Any)(s: Subst) =
    unify(t1, t2, s) match {
      case Some(s2) => succeed(s2)
      case None => fail(s)
    }

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

  /* produce at most n results */
  /* Note: sometimes the syntactic sugar of omitting . for method access fails */
  def run(n: Int, v: Var)(g: Goal) = {
    val allres = g(empty_s)  map {s: Subst => reify(walk_*(v, s)) }
    (if (n < 0) allres else allres take n).toList
  }
}

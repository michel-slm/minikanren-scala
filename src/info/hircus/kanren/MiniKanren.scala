package info.hircus.kanren

object MiniKanren {
  import java.util.HashMap

  /* Monads */
  def succeed(s: Subst): Stream[Subst] =
    Stream.cons(s, Stream.empty)
  def fail(s: Subst): Stream[Subst] = Stream.empty

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
    x.isInstanceOf[List[Any]] && x != Nil

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
      val ls = v1.asInstanceOf[List[Any]]
      walk_*(ls.head, s) :: walk_*(ls.tail, s).asInstanceOf[List[Any]]
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
      val ls = v1.asInstanceOf[List[Any]]
      reify_s(ls.tail, reify_s(ls.head, s))
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
      val ls1 = t1.asInstanceOf[List[Any]]
      val ls2 = t2.asInstanceOf[List[Any]]

      unify(ls1.head, ls2.head, s) match {
	case None => return None
	case Some(s2: Subst) =>
	  return unify(ls1.tail, ls2.tail, s2)
      }
    }
    else if (t1 == t2) return Some(s)
    else return None
  }
}

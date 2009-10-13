package info.hircus.kanren.tests

import org.scalacheck._
import info.hircus.kanren.MiniKanren._

object SubstSpecification extends Properties("Substitution") {
  import Prop.forAll
  
  /* Utility function */
  def remove_right_dups[A](s: List[A]): List[A] = {
    if (s.isEmpty) s
    else s.head :: remove_right_dups(s.tail.remove({_ == s.head}))
  }

  property("freshvar") = forAll { (vstr: String) =>
    val v = make_var(Symbol(vstr))
    walk_*(v,reify_s(v, empty_s)) == Symbol("_.0")
  }
  
  /* for a list containing at least one variable, the reified substitution
   * contains as many bindings as there are unique variables
   */
  property("freshvarls") = forAll { (n: Int, ls: List[Int]) =>
    import info.hircus.kanren.MKLib._

    val vars = (n::ls) map { n: Int => make_var(Symbol(n.toString)) }
    val pvars = list2pair(vars).asInstanceOf[(Any,Any)]

    val s = reify_s(pvars, empty_s)
    
    val unique_vars = remove_right_dups(vars)
    
    ( (s.toList == reify_s(pvars._2,
			   reify_s(pvars._1, empty_s)).toList) &&
     unique_vars.length == s.length &&
     pair2list(walk_*(list2pair(unique_vars), s)) ==
       ((0 until s.length) map { reify_name(_) } toList) )
  }
  
}

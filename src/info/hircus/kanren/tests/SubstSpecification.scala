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
    val v = Var(Symbol(vstr))
    walk_*(v,reify_s(v, empty_s)) == Symbol("_.0")
  }
  
  /* for a list containing at least one variable, the reified substitution
   * contains as many bindings as there are unique variables
   */
  property("freshvarls") = forAll { (n: Int, ls: List[Int]) =>
    
    val vars = (n::ls) map { n: Int => Var(Symbol(n.toString)) }
    
    val s = reify_s(vars, empty_s)
    
    val unique_vars = remove_right_dups(vars)
    
    (s.toList == reify_s(vars.tail, reify_s(vars.head, empty_s)).toList) &&
      unique_vars.length == s.length &&
      walk_*(unique_vars, s) == ((0 until s.length) map { reify_name(_) } toList)
  }
  
}

package info.hircus.kanren.tests

import org.scalacheck._
import info.hircus.kanren.MiniKanren._

object UnifySpecification extends Properties("Unification") {
  import Prop.forAll
  
  /* Utility function */
  def remove_right_dups[A](s: List[A]): List[A] = {
    if (s.isEmpty) s
    else s.head :: remove_right_dups(s.tail.remove({_ == s.head}))
  }

  property("bindonce") = forAll { n: Int =>
    val v = Var('v)
    (for {
      s <- unify(v, n, empty_s)
      res <- lookup(v, s)
    } yield res)  match {
      case Some(x) => x == n
      case None => false
    }
  }
  
  property("bindtwice") = forAll { (vstr: String, m: Int, n: Int) =>
    val v = Var(Symbol(vstr))
    (for {
      s1 <- unify(v, m, empty_s)
      s2 <- unify(v, n, s1)
      res <- lookup(v, s2)
    } yield res) match {
      case Some(_) => m==n
      case None => true
    }
  }
  
}

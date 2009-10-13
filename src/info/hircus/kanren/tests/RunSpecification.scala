package info.hircus.kanren.tests

import org.scalacheck._
import info.hircus.kanren.MiniKanren._
import info.hircus.kanren.MKLogic._
import info.hircus.kanren.MKLib._

object RunSpecification extends Properties("Run") {
  import Prop.forAll

  val v = make_var('v)
  val w = make_var('w)
  
  property("==") = forAll { n: Int =>
    run(1, v)(mkEqual(v, n)) == List(n)
  }

  property("==*") = forAll { n: Int =>
    run(-1, v)(mkEqual(v, n)) == List(n)
  }

  property("all0") = run(-1, v)(all(Nil)) == List(Symbol("_.0"))
  property("all1") = forAll { n: Int => run(-1, v)(all(List(mkEqual(v, n)))) == List(n) }
  property("all*") = forAll { (m: Int, n: Int) =>
    run(-1, v)(all(List(mkEqual(v, n), mkEqual(w, m)))) == n &&
    run(-1, w)(all(List(mkEqual(v, n), mkEqual(w, m)))) == m
  }

  property("null") = forAll { n: Int =>
    val x = make_var('x)
    if (n <= 0) true
    else run(n, x)(null_o(x)) == List(Nil)
  }

  property("car") = forAll { (m: Int, n: Int) =>
    val x = make_var('x)
    run(-1, x)(car_o( (m, n), x )) == List(m) 
  }

  property("cdr") = forAll { (m: Int, n: Int) =>
    val x = make_var('x)
    run(-1, x)(cdr_o( (m, n), x )) == List(n) 
  }

  property("listgen") = forAll { n: Int =>
    val ls = make_var('ls)
    if (n <= 0 || n > 100) true
    else {
      val res = run(n, ls)(list_o(('a, ('b, ('c, ls)))))
      res.length == n
    }
    
  }
}

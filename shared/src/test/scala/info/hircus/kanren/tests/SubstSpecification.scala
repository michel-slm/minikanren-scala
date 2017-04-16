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

package info.hircus.kanren.tests

import org.scalacheck._
import info.hircus.kanren.MiniKanren._

object SubstSpecification extends Properties("Substitution") {
  import Prop.forAll
  
  /* Utility function */
  def remove_right_dups[A](s: List[A]): List[A] = {
    if (s.isEmpty) s
    else s.head :: remove_right_dups(s.tail.filterNot({_ == s.head}))
  }

  property("freshvar") = forAll { (vstr: String) =>
    val v = make_var(Symbol(vstr))
    walk_*(v,reify_s(v, empty_s)) == Symbol("_.0")
  }
  
  /* for a list containing at least one variable, the reified substitution
   * contains as many bindings as there are unique variables
   */
  property("freshvarls") = forAll { (n: Int, ls: List[Int]) =>
    import info.hircus.kanren.Prelude._

    val vars = (n::ls) map { n: Int => make_var(Symbol(n.toString)) }
    val pvars = list2pair(vars).asInstanceOf[(Any,Any)]

    val s = reify_s(pvars, empty_s)
    
    val unique_vars = remove_right_dups(vars)
    
    ( (s  == reify_s(pvars._2,
			   reify_s(pvars._1, empty_s))) &&
     unique_vars.length == s.length &&
     pair2list(walk_*(list2pair(unique_vars), s)) ==
       ((0 until s.length) map { reify_name(_) } toList) )
  }
  
}

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

object UnifySpecification extends Properties("Unification") {
  import Prop.forAll

  val v = make_var('v)
  val w = make_var('w)

  /* Utility function */
  def remove_right_dups[A](s: List[A]): List[A] = {
    if (s.isEmpty) s
    else s.head :: remove_right_dups(s.tail.remove({_ == s.head}))
  }

  property("bindonce") = forAll { n: Int =>
    val v = make_var('v)
    (for {
      s <- empty_s.unify(v, n)
      res <- s.lookup(v)
    } yield res)  match {
      case Some(x) => x == n
      case None => false
    }
  }
  
  property("bindtwice") = forAll { (vstr: String, m: Int, n: Int) =>
    val v = make_var(Symbol(vstr))
    (for {
      s1 <- empty_s.unify(v, m)
      s2 <- s1.unify(v, n)
      res <- s2.lookup(v)
    } yield res) match {
      case Some(_) => m==n
      case None => true
    }
  }

  property("pairs") = forAll { (m:Int, n: Int) =>
    def pairGoal: Goal =
      (v, w) === (m, n)
    
    run(-1, v)(pairGoal) == List(m) &&
    run(-1, w)(pairGoal) == List(n) }

  property("=/= #1") = forAll { n:Int =>
    crun(-1, v)(v =/= n, v === n) == Nil }

}


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
import info.hircus.kanren.Prelude._

object BranchingSpecification extends Properties("Branching") {
  import Prop.forAll

  val v = make_var('v)
  val w = make_var('w)
  
  property("fail-then-never") = run(1, v)(all(fail, never_o)) == Nil

  property("always-first") =
    run(5, v)(all(always_o, true === v)) == (
      (for { x <- 0 until 5 } yield true) toList )

  property("always-second") =
    run(5, v)(all(true === v, always_o)) == (
      (for { x <- 0 until 5 } yield true) toList )

    
  property("cond_i #1") =
    run(5, v)(both(if_i(false === v, always_o,
			if_i(true === v, always_o,
			     fail)),
		   true === v)) == ( (for { x <- 0 until 5 } yield true) toList )

  property("cond_i #2") =
    run(5, v)(both(all_i(if_e(false === v, succeed,
			      true === v),
			 always_o),
		   true === v)) == (
		     (for { x <- 0 until 5 } yield true) toList )

}

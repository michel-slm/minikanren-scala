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

object Examples {
  import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.Prelude._
  import info.hircus.kanren.MKMath._

  def add_len_o(a: Any, b: Any, c: Any): Goal = {
    val a1 = make_var('a1)
    val ar = make_var('ar)
    val c1 = make_var('c1)
    val cr = make_var('cr)

    if_e(mkEqual(a, Nil), eq_len_o(b, c),
	 if_e(both(mkEqual(a, (a1,ar)), mkEqual(c, (c1,cr))),
	      add_len_o(ar,b,cr),
	      fail))
  }

  def palprod_o(q: Any): Goal = {
    val a = make_var('a)
    val a9091 = make_var('a9091)
    val b = make_var('b)
    val b910 = make_var('b910)
    val c = make_var('c)
    val c100 = make_var('c100)
    val t1 = make_var('t1)
    val sum = make_var('sum)
    val k = make_var('k)

    all(digit_o(a),
	pos_o(a),
	mul_o(a, build_num(9091), a9091),
	digit_o(b),
	mul_o(b, build_num(910), b910),
	add_o(a9091, b910, t1),
	digit_o(c),
	mul_o(c, build_num(100), c100),
	add_o(t1, c100, sum),
	{ s: Subst => {
	  val the_sum = walk_*(sum, s)
	  if (!the_sum.isInstanceOf[Var]) println(11*read_num(the_sum))
	  succeed(s)
	}},
	if_e(eq_len_o(k, build_num(8)), lt_o(build_num(9), k),
	     if_e(eq_len_o(k, build_num(16)), succeed,
		  if_e(eq_len_o(k, build_num(32)), succeed,
		       if_e(eq_len_o(k, build_num(64)), lt_o(k, build_num(91)),
			    fail)))),
	once({ s: Subst => {
	  val the_sum = walk_*(sum, s)
	  val xyz = make_var('xyz)
	  all(lt_len_o(xyz, build_num(1024)),
	      lt_len_o(build_num(32), xyz),
	      if_e(add_len_o(k, xyz, sum), succeed,
		   add_len_o(k, xyz, (0,sum))),
	      mul_o(k, xyz, sum),
	      lt_o(xyz, build_num(1000)),
	      lt_o(build_num(99), xyz))(s) }}),
	mkEqual(q, sum))
  }
}

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

package info.hircus.kanren.examples

object SendMoreMoney {
  import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.Prelude._
  import info.hircus.kanren.MKMath._

  private def common_prefix_o(l: Any, l1: Any, l2: Any): Goal = {
    if_i(null_o(l), succeed,
	 { s: Subst => {
	   val x   = make_var('x)
	   val ls  = make_var('ls)
	   val l1s = make_var('l1s)
	   val l2s = make_var('l2s)

	   all(l  === (x, ls),
	       l1 === (x, l1s),
	       l2 === (x, l2s),
	       common_prefix_o(ls, l1s, l2s))(s) } })
  }

  def reme(x: Any, l: Any, lo: Any): Goal = {
    val l1 = make_var('l1)
    val l2 = make_var('l2)

    all_i(common_prefix_o(l1, l, lo),
	  append_o(l1, (x,l2), l),
	  append_o(l1, l2, lo))
  }


  def solve_puzzle(q: Any): Goal = {
    val all_digits = list2pair(((0 to 10) toList) map build_num)
    val ten = build_num(10)

    def make_number(digits: Any, n: Any) = {
      def loop(digits: Any, acc: Any): Goal = {
	if_i(digits === Nil, n === acc,
	     { s: Subst => {
	       val d = make_var('d)
	       val rest = make_var('rest)
	       val acc1 = make_var('acc1)
	       val acc2 = make_var('acc2)

	       all_i(digits === (d, rest),
		     mul_o(acc, ten, acc1),
		     add_o(acc1, d, acc2),
		     loop(rest, acc2))(s) } } )
      }
      loop(digits, Nil)
    }

    def choose_digits(digits: Any,
		      all_digits: Any,
		      remained_digits: Any): Goal = {
      if_i(digits === Nil, all_digits === remained_digits,
	   { s: Subst => {
	     val d = make_var('d)
	     val rest = make_var('rest)
	     val set1 = make_var('set1)
	     
	     all_i(digits === (d, rest),
		   reme(d, all_digits, set1),
		   choose_digits(rest, set1, remained_digits))(s) } })
    }

    /**
     * d1 + d2 + ci = do + 10*co
     * c1 and co can only be either 0 or 1
     */
    def add_carry(ci: Any, d1: Any, d2: Any, d_o: Any, co: Any) = {
      val d11 = make_var('d11)
      val dr  = make_var('dr)

      all(if_e(ci === Nil, succeed,
	       ci === (1,Nil)),
	  add_o(ci, d1, d11),
	  add_o(d11, d2, dr),
	  if_e(dr === d_o, co === Nil,
	       if_e(add_o(d_o, ten, dr), co === (1,Nil),
		    fail)))
    }

    val s = make_var('s)
    val e = make_var('e)
    val n = make_var('n)
    val d = make_var('d)
    val m = make_var('m)
    val o = make_var('o)
    val r = make_var('r)
    val y = make_var('y)
    val send = make_var('send)
    val more = make_var('more)
    val money = make_var('money)
    val c1 = make_var('c1)
    val c2 = make_var('c2)
    val c3 = make_var('c3)
    val rd1 = make_var('rd1)
    val rd2 = make_var('rd2)
    val rd3 = make_var('rd3)

    all_i(choose_digits((m,(s,(o,Nil))), all_digits, rd1),
	  pos_o(s),
	  pos_o(m),
	  add_carry(c3,  s,m,o,m),
	  choose_digits((e,(n,Nil)), rd1, rd2),
	  add_carry(c2,  e,o,n,c3),
	  choose_digits((r,(d,(y,Nil))), rd2, rd3),
	  add_carry(Nil, d,e,y,c1),
	  add_carry(c1,  n,r,e,c2),
	  // verify
	  make_number((s,(e,(n,(d,Nil)))), send),
	  make_number((m,(o,(r,(e,Nil)))), more),
	  make_number((m,(o,(n,(e,(y,Nil))))), money),
	  add_o(send, more, money),

	  { s: Subst => {
	    val the_send  = walk_*(send, s)
	    val the_more  = walk_*(more, s)
	    val the_money = walk_*(money, s)

	    (q === (List(the_send, the_more, the_money) map read_num))(s) } }
	)
  }
}

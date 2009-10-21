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

/**
 * Mini Kanren math relations
 * This will be merged with Prelude once all operations are implemented
 */
object MKMath {
  import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.Prelude._

  /**
   * (x XOR y) === r
   *
   * @param x a bit
   * @param y a bit
   * @param r a bit
   */
  def bit_xor_o(x: Any, y: Any, r: Any): Goal = {
    if_e(both(mkEqual(0,x), mkEqual(0,y)), mkEqual(0,r),
	 if_e(both(mkEqual(1,x), mkEqual(0,y)), mkEqual(1,r),
	      if_e(both(mkEqual(0,x), mkEqual(1,y)), mkEqual(1,r),
		   if_e(both(mkEqual(1,x), mkEqual(1,y)), mkEqual(0,r),
			fail)))) }

  /**
   * (x AND y) === r
   *
   * @param x a bit
   * @param y a bit
   * @param r a bit
   */
  def bit_and_o(x: Any, y: Any, r: Any): Goal = {
    if_e(both(mkEqual(0,x), mkEqual(0,y)), mkEqual(0,r),
	 if_e(both(mkEqual(1,x), mkEqual(0,y)), mkEqual(0,r),
	      if_e(both(mkEqual(0,x), mkEqual(1,y)), mkEqual(0,r),
		   if_e(both(mkEqual(1,x), mkEqual(1,y)), mkEqual(1,r),
			fail)))) }
  
  /**
   * Given the bits x, y, r, and c, satisfies x + y = r + 2*c
   *
   * @param x a bit
   * @param y a bit
   * @param r x+y % 2
   * @param c x+y >> 1
   */
  def half_adder_o(x: Any, y: Any, r: Any, c: Any): Goal = {
    both(bit_xor_o(x,y,r),
	 bit_and_o(x,y,c))
  }

  /**
   * Given the bits b, x, y, r, and c,satisfies b + x + y == r + 2*c
   *
   * @param b previous carry
   * @param x a bit
   * @param y a bit
   * @param r b+x+y % 2
   * @param c b+x+y >> 1
   */
  def full_adder_o(b: Any, x: Any, y: Any, r: Any, c: Any): Goal = {
    val w  = make_var('w)
    val xy = make_var('xy)
    val wz = make_var('wz)

    all(half_adder_o(x,y,w,xy),
	half_adder_o(w,b,r,wz),
	bit_xor_o(xy,wz,c))
  }

  /**
   * Build a Kanren number from an integer
   *
   * @param n an integer
   * @return a Kanren number
   */
  def build_num(n: Int): Any = {
    if (n==0) Nil
    else ( (n%2), build_num(n >> 1) )
  }

  /**
   * Read a Kanren number as an integer
   *
   * @param  n a Kanren number
   * @return an integer
   */
  def read_num(n: Any): Int = n match {
    case Nil => 0
    case (x, p) => x.asInstanceOf[Int] + (read_num(p) << 1)
  }

  /* "Predicates" */
  /**
   * succeeds when n represents a positive number
   *
   * @param n a bitlist number
   */
  def pos_o(n: Any): Goal = {
    val a = make_var('a)
    val d = make_var('d)

    mkEqual((a,d), n)
  }

  /**
   * succeeds when n represents a number > 1
   *
   * @param n a bitlist number
   */
  def gt1_o(n: Any): Goal = {
    val a  = make_var('a)
    val ad = make_var('ad)
    val dd = make_var('dd)

    mkEqual((a,(ad,dd)), n)
  }


  /**
   * Holds if a is a digit (i.e. 0 to 9)
   *
   * @param a a bitlist number
   */
  def digit_o(a: Any): Goal = {
    cond_e((mkEqual(a, build_num(0)), succeed),
	   (mkEqual(a, build_num(1)), succeed),
	   (mkEqual(a, build_num(2)), succeed),
	   (mkEqual(a, build_num(3)), succeed),
	   (mkEqual(a, build_num(4)), succeed),
	   (mkEqual(a, build_num(5)), succeed),
	   (mkEqual(a, build_num(6)), succeed),
	   (mkEqual(a, build_num(7)), succeed),
	   (mkEqual(a, build_num(8)), succeed),
	   (mkEqual(a, build_num(9)), succeed))
  }

  /**
   * Holds if both n and m are zero
   * or if floor(log2(n)) == floor(log2(m))
   *
   * @param n a bitlist number
   * @param m a bitlist number
   */
  def eq_len_o(n: Any, m: Any): Goal = {
    if_e(mkEqual(Nil, n), mkEqual(Nil, m),
	 eq_len_o_aux(n, m))
  }

  private def eq_len_o_aux(n: Any, m: Any): Goal = {
    if_e(mkEqual((1,Nil), n), mkEqual((1,Nil), m),
	 { s: Subst => {
	   val x  = make_var('x)
	   val y  = make_var('y)
	   val any1  = make_var('any1)
	   val any2 = make_var('any2)
	   all(mkEqual((any1,x), n), pos_o(x),
	       mkEqual((any2,y), m), pos_o(y),
	       eq_len_o_aux(x,y))(s)
	 } })
  }

  def lt_len_o(n: Any, m: Any): Goal = {
    if_e(mkEqual(Nil,n), pos_o(m),
	 if_e(mkEqual((1,Nil),n), gt1_o(m),
	      { s: Subst => {
		val a = make_var('a)
		val x = make_var('x)
		val b = make_var('b)
		val y = make_var('y)

		all(mkEqual((a,x),n), pos_o(x),
		    mkEqual((b,y),m), pos_o(y),
		    lt_len_o(x,y))(s)
	      } }))
  }

  /**
   * Holds if n < m
   *
   * @param n a bitlist number
   * @param m a bitlist number
   */
  def lt_o(n: Any, m: Any): Goal = {
   cond_i((lt_len_o(n,m), succeed),
	  (eq_len_o(n,m),
	   { s: Subst => {
	     val x = make_var('x)
	     both(pos_o(x), add_o(n, x, m))(s)
	   } }))
  }
  
  /* Math operations */
  def gen_adder_o(d:Any, n: Any, m: Any, r: Any): Goal = {
    val a = make_var('a)
    val b = make_var('b)
    val c = make_var('c)
    val e = make_var('e)
    val x = make_var('x)
    val y = make_var('y)
    val z = make_var('z)

    all(mkEqual((a,x),n),
	mkEqual((b,y),m), pos_o(y),
	mkEqual((c,z),r), pos_o(z),
	all_i(full_adder_o(d,a,b,c,e),
	      adder_o(e,x,y,z)))
  }

  def adder_o(d: Any, n: Any, m: Any, r: Any): Goal = {
    if_i(both(mkEqual(0,d), mkEqual(Nil,m)), mkEqual(n,r),
	 if_i(all(mkEqual(0,d), mkEqual(Nil,n), mkEqual(m,r)), pos_o(m),
	      if_i(both(mkEqual(1,d), mkEqual(Nil,m)), adder_o(0, n, (1,Nil), r),
		   if_i(all(mkEqual(1,d), mkEqual(Nil,n), pos_o(m)), adder_o(0, (1,Nil), m, r),
			if_i(both(mkEqual((1,Nil),n), mkEqual((1,Nil),m)),
			     { s: Subst => {
			       val a = make_var('a)
			       val c = make_var('c)
			       both(mkEqual((a,(c,Nil)), r),
				    full_adder_o(d,1,1,a,c))(s)
			     } },
			     if_i(mkEqual((1,Nil),n), gen_adder_o(d,n,m,r),
				  if_i(all(mkEqual((1,Nil),m), gt1_o(n), gt1_o(r)),
				       adder_o(d, (1,Nil), n, r),
				       if_i(gt1_o(n), gen_adder_o(d,n,m,r),
					    fail))))))))
  }

  /**
   * (n + m) === k
   *
   * @param n a Kanren number
   * @param m a Kanren number
   * @param k a Kanren number
   */
  def add_o(n: Any, m: Any, k: Any): Goal = adder_o(0, n, m, k)

  /**
   * (n - m) === k
   *
   * @param n a Kanren number
   * @param m a Kanren number
   * @param k a Kanren number
   */
  def sub_o(n: Any, m: Any, k: Any): Goal = adder_o(0, m, k, n)

  /* Multiplication */

  /**
   * The multiplication relation
   *
   * @param n a bitlist number
   * @param m a bitlist number
   * @param p the product bitlist
   */
  def mul_o(n: Any, m: Any, p: Any): Goal = {
    if_i(mkEqual(Nil,n), mkEqual(Nil, p),
	 if_i(both(pos_o(n), mkEqual(Nil,m)), mkEqual(Nil,p),
	      if_i(both(mkEqual((1,Nil),n), pos_o(m)), mkEqual(m,p),
		   if_i(both(gt1_o(n), mkEqual((1,Nil),m)), mkEqual(n,p),
			if_i({ s: Subst => {
			  val x = make_var('x)
			  val z = make_var('z)
			  all(mkEqual((0,x),n), pos_o(x),
			      mkEqual((0,z),p), pos_o(z),
			      gt1_o(m),
			      mul_o(x,m,z))(s) }}, succeed,
			     if_i({ s: Subst => {
			       val x = make_var('x)
			       val y = make_var('y)
			       all(mkEqual((1,x),n), pos_o(x),
				   mkEqual((0,y),m), pos_o(y),
				   mul_o(m,n,p))(s) }}, succeed,
				  if_i({ s: Subst => {
				    val x = make_var('x)
				    val y = make_var('y)
				    all(mkEqual((1,x),n), pos_o(x),
					mkEqual((1,y),m), pos_o(y),
					odd_mul_o(x,n,m,p))(s) }}, succeed,
				       fail)))))))
  }

  def odd_mul_o(x: Any, n: Any, m: Any, p: Any) = {
    val q = make_var('q)
    all(bound_mul_o(q,p,n,m),
	mul_o(x,m,q),
	add_o((0,q), m, p))
  }

  def bound_mul_o(q: Any, p: Any, n: Any, m: Any): Goal = {
    if_e(null_o(q), pair_o(p),
	 { s: Subst => {
	   val x = make_var('x)
	   val y = make_var('y)
	   val z = make_var('z)
	   all(cdr_o(q,x),
	       cdr_o(p,y),
	       if_i(both(null_o(n), cdr_o(m, z)),
		    bound_mul_o(x,y,z,Nil),
		    both(cdr_o(n,z), bound_mul_o(x,y,z,m))))(s) } } )
  }
			  
}

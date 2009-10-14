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

object MKMath {
  import info.hircus.kanren.MiniKanren._

  def bit_xor_o(x: Any, y: Any, r: Any): Goal = {
    if_e(both(mkEqual(0,x), mkEqual(0,y)), mkEqual(0,r),
	 if_e(both(mkEqual(1,x), mkEqual(0,y)), mkEqual(1,r),
	      if_e(both(mkEqual(0,x), mkEqual(1,y)), mkEqual(1,r),
		   if_e(both(mkEqual(1,x), mkEqual(1,y)), mkEqual(0,r),
			fail)))) }

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


  def build_num(n: Int): Any = {
    if (n==0) Nil
    else ( (n%2), build_num(n >> 1) )
  }

  def read_num(n: Any): Int = n match {
    case Nil => 0
    case (x, p) => x.asInstanceOf[Int] + (read_num(p) << 1)
  }

  def pos_o(n: Any): Goal = {
    val a = make_var('a)
    val d = make_var('d)

    mkEqual((a,d), n)
  }

  def gt1_o(n: Any): Goal = {
    val a  = make_var('a)
    val ad = make_var('ad)
    val dd = make_var('dd)

    mkEqual((a,(ad,dd)), n)
  }

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

  def add_o(n: Any, m: Any, k: Any): Goal = adder_o(0, n, m, k)
  def sub_o(n: Any, m: Any, k: Any): Goal = adder_o(0, m, k, n)

}

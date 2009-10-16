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
import info.hircus.kanren.MKMath._
import info.hircus.kanren.Prelude._

object MathSpecification extends Properties("Math") {
  import Prop.forAll

  private val MIN_INT=0
  private val MAX_INT=1000000000

  private def pairGen(min: Int, max: Int) = for {
    n <- Gen.choose(min, max)
    m <- Gen.choose(min, max)
  } yield (n,m)

  private def tripleGen(min: Int, max: Int) = for {
    x <- Gen.choose(min, max)
    y <- Gen.choose(min, max)
    z <- Gen.choose(min, max)
  } yield (x,y,z)

  val b = make_var('b)
  val x = make_var('x)
  val y = make_var('y)
  val r = make_var('r) // remainder
  val c = make_var('c) // carry
  val s = make_var('s) // sum
  
  property("bit-xor-o 0") = run(-1, s)(both(bit_xor_o(x,y,0), mkEqual((x,y), s))) == List((0,0),(1,1))
  property("bit-xor-o 1") = run(-1, s)(both(bit_xor_o(x,y,1), mkEqual((x,y), s))) == List((1,0),(0,1))
  property("bit-and-o 0") = run(-1, s)(both(bit_and_o(x,y,0), mkEqual((x,y), s))) == List((0,0),(1,0),(0,1))
  property("bit-and-o 1") = run(-1, s)(both(bit_and_o(x,y,1), mkEqual((x,y), s))) == List((1,1))

  property("half-adder-o") = {
    ((run(-1, s)(both(half_adder_o(x,y,r,c),
                     mkEqual(list2pair(List(x,y,r,c)), s))) map pair2list _ )
     ==
       List(List(0,0,0,0),
            List(1,0,1,0),
            List(0,1,1,0),
            List(1,1,0,1)) ) }

  property("full-adder-o") = {
    ((run(-1, s)(both(full_adder_o(b,x,y,r,c),
                      mkEqual(list2pair(List(b,x,y,r,c)), s))) map pair2list _ )
     ==
       List(List(0,0,0,0,0),
            List(1,0,0,1,0),
            List(0,1,0,1,0),
            List(1,1,0,0,1),
            List(0,0,1,1,0),
            List(1,0,1,0,1),
            List(0,1,1,0,1),
            List(1,1,1,1,1)) ) }

  property("gen-adder-o") = {
    ((run(-1,s)(gen_adder_o(1, list2pair(List(0,1,1)),
                          list2pair(List(1,1)), s))) map pair2list _) == List(List(0,1,0,1))
  }

  property("adder-o") = {
    val res = ((run(-1, s)(both(adder_o(0, x, y, list2pair(List(1,0,1))),
                               mkEqual((x,(y,Nil)), s)))) map pair2list _ )
    ( (res map { l: List[Any] => l map pair2list _ } )
     ==
       List(List(List(1,0,1), Nil),
            List(Nil, List(1,0,1)),
            List(List(1), List(0,0,1)),
            List(List(0,0,1), List(1)),
            List(List(1,1), List(0,1)),
            List(List(0,1), List(1,1))) )
  }

  property("build-read") = forAll(Gen.choose(MIN_INT, MAX_INT)) { n =>
    read_num(build_num(n)) == n }

  property("add_o") = forAll(pairGen(MIN_INT, MAX_INT)) { p =>
    run(-1, s)(add_o(build_num(p _1),
                     build_num(p _2),
                     s)) == List(build_num(p._1+p._2)) }

  property("sub_o") = forAll(pairGen(MIN_INT, MAX_INT)) { p =>
    run(-1, s)(sub_o(build_num(p _1),
                     build_num(p._1+p._2+1),
                     s)) == Nil }

  property("mul_o prod") = forAll(pairGen(0,100)) { p => p match {
    case (x,y) => {
      val bx = build_num(x)
      val by = build_num(y)
      val bz = build_num(x*y)
      run(-1, s)(mul_o(bx, by, s)) == List(bz)
    } }}

  /* same behavior as Scheme MK */
  property("mul_o zero") = {
    val ans = List(Nil, (Symbol("_.0"), Symbol("_.1")))
    run(-1, s)(mul_o(s, Nil, Nil)) == ans &&
    run(-1,s)(mul_o(Nil,s,Nil)) == List(Symbol("_.0"))
  }

  /* next two are very slow; must debug */
  property("mul_o mul0") = forAll(pairGen(1,10)) { p => p match {
    case (x,y) => {
      val bx = build_num(x)
      val by = build_num(y)
      val bz = build_num(x*y)
      run(-1, s)(mul_o(s, by, bz)) == List(bx)
    } }}

  property("mul_o mul1") = forAll(pairGen(1,10)) { p => p match {
    case (x,y) => {
      val bx = build_num(x)
      val by = build_num(y)
      val bz = build_num(x*y)
      run(-1, s)(mul_o(bx, s, bz)) == List(by)
    } }}

}

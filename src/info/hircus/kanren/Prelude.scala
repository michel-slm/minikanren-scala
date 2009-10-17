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

object Prelude {
  import info.hircus.kanren.MiniKanren._

  /* Control operators */
  def once(g: Goal): Goal = {
    if_u(g, succeed,
	 fail)
  }

  /* Lists */

  /**
   * Utility function to convert a Scala linked list to a
   * pair that is more digestible
   *
   * @param l a Scala list
   * @return a list made of nested pairs
   */
  def list2pair(l: List[Any]): Any = l match {
    case Nil => Nil
    case h :: tl => (h, list2pair(tl))
  }

  /**
   * Utility function to convert back from nested pairs to a Scala list
   *
   * @param p a list made of nested pairs, Nil-terminated
   * @return a Scala list
   * @todo might have to return an exception for improper lists
   */
  def pair2list(p: Any): List[Any] = p match {
    case Nil => Nil
    case (h, tl) => h :: pair2list(tl)
  }

  
  /**
   * A relation unifying the head of the pair 'p' with 'a'
   *
   * @param p something pair-able
   * @param a anything
  */ 
  def car_o(p: Any, a: Any): Goal = {
    val d = make_var('d)
    mkEqual( (a, d), p )
  }

  /**
   * A relation unifying the tail of the pair 'p' with 'd'
   *
   * @param p something pair-able
   * @param d anything
   */
  def cdr_o(p: Any, d: Any): Goal = {
    val a = make_var('a)
    mkEqual( (a, d), p )
  }

  /**
   * A relation unifying p with a fresh pair of variables
   *
   * @param p something pair-able
   */
  def pair_o(p: Any): Goal = {
    val a = make_var('a)
    val d = make_var('d)
    mkEqual( (a, d), p )
  }

  /**
   * A relation that unifies 'x' with the empty list
   *
   * @param x something null-able
   */
  def null_o(x: Any): Goal = {
    mkEqual( Nil, x )
  }

  /**
   * A relation that unifies l with a Kanren list
   * If l is fresh, this is actually a list generator
   *
   * @param l something list-able
   */
  def list_o (l: Any): Goal =
    if_e(null_o(l), succeed,
	 if_e(pair_o(l), { s: Subst =>
           val d = make_var('d)
	   all(cdr_o(l, d),
               list_o(d))(s) },
	      fail))

  /**
   * A relation that unifies 'x' with an element from 'l'
   * If x is fresh, collecting all results  yield all the elements of l
   * If 'l' is fresh, generates all possible lists containing 'x'
   * Otherwise, the only possible result is if 'x' matches an element in 'l'
   *
   * @param x any
   * @param l something list-able
   */
  def member_o(x: Any, l: Any): Goal =
    if_e(null_o(l), fail,
         if_e(car_o(l, x), succeed,
	      {s: Subst =>
		val d = make_var('d)
                all(cdr_o(l, d),
                member_o(x, d))(s)
               } ))

  /**
   * see page 77 of Reasoned Schemer
   *
   * @param g a Goal
   * @return a goal that succeeds if 'g' succeeds, and otherwise has
   *         no value (bottom)
   */
  def any_o(g: Goal): Goal = if_e(g, succeed, any_o(g))

  /**
   * Bottom: this is a goal that never terminates if evaluated
   * see page 77 of Reasoned Schemer
   */
  def never_o: Goal = any_o(fail)

  /**
   * Always: a goal that can succeed an infinite number of times
   * see page 77-78 of Reasoned Schemer
   */
  def always_o: Goal = any_o(succeed)

}

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

import info.hircus.kanren.MiniKanren._

object Substitution {

  /**
   * An empty simple substitution
   */
  object EmptySubst extends Subst {
    /**
     * Extending an empty substitution always succeeds, producing a simple substitution
     * with one binding, v -> x
     *
     * @param v a logical variable
     * @param x a value to bind x to
     */
    def extend(v: Var, x: Any) = Some(SimpleSubst(v,x,this))
    /**
     * Looking up in an empty substitution always fails
     *
     * @param v a logical variable
     */
    def lookup(v: Var) = None
    /**
     * The length of an empty substitution is zero
     */
    def length: Int = 0
  }

  /**
   * A non-empty simple substitution
   */
  case class SimpleSubst(v: Var, x: Any, s: Subst) extends Subst {
    /**
     * Extending a simple substitution always succeeds, producing a new substitution
     * linked with the current one
     *
     * @param v a logical variable
     * @param x a value to bind to x
     */
    def extend(v: Var, x: Any) = Some(SimpleSubst(v,x,this))
    /**
     * Looking up a variable succeeds immediately if it is at the head of the substitution.
     * Otherwise, the linked substitution is queried.
     *
     * @param v a logical variable
     */
    def lookup(v: Var) = if (this.v == v) Some(x) else s.lookup(v)

    /**
     * The length of a non-empty substitution is one more than its linked substitution
     */
    def length: Int = 1 + s.length
  }

  abstract class ConstraintSubst extends Subst {
    /**
     * In a constrained substitution, two walked terms are only unifiable if neither are listed in
     * the other's constraints
     */
    override def unify(term1: Any, term2: Any): Option[Subst] = {
      val v1 = walk(term1, this)
      val v2 = walk(term2, this)

      if (v1.isInstanceOf[Var] && (this.constraints(v1.asInstanceOf[Var]) contains v2)) None
      else if (v2.isInstanceOf[Var] && (this.constraints(v2.asInstanceOf[Var]) contains v1)) None
      else super.unify(v1, v2)
    }
  }

  private def c_lookup(v: Var, c: Constraints): List[Any] = c match {
    case Nil => Nil
    case (w, cls) :: c2 => if (v==w) cls else c_lookup(v, c2)
  }

  private def c_insert(v: Var, x: Any, c: Constraints): Constraints = c match {
    case Nil => List((v, List(x)))
    case (w, cls) :: c2 => if (v==w) ((w, if (cls contains x) cls
					  else x::cls) :: c2)
			   else (w,cls) :: c_insert(v,x,c2)
  }


  case class ConstraintSubst0(c: Constraints) extends Subst {
    /**
     * extending a constraint substitution creates a new constraint substitution
     * with the extension done in the simple substitution part
     */
    def extend(v: Var, x: Any) =
      if (this.constraints(v) contains x) None
      else Some(ConstraintSubstN(SimpleSubst(v,x,this), c))

    override def c_extend(v: Var, x: Any) = ConstraintSubst0(c_insert(v,x,c))

    /**
     * Looking up a variable in an empty constraint substitution always returns None
     *
     * @param v a logical variable
     * @return None
     */
    def lookup(v: Var) = None
    override def constraints(v: Var) = c_lookup(v, c)
    /**
     * The length of an empty constraint substitution is zero
     */
    def length: Int = 0
  }

  case class ConstraintSubstN(s: SimpleSubst, c: Constraints) extends Subst {
    /**
     * Constraint checking is performed here, since it is not needed with
     * simple substitutions. Doing it in unify would be less efficient
     */
    def extend(v: Var, x: Any) =
      if (this.constraints(v) contains x) None
      else Some(ConstraintSubstN(SimpleSubst(v,x,s), c))
  
    override def c_extend(v: Var, x: Any) = ConstraintSubstN(s, c_insert(v,x,c))

    /**
     * Looking up a variable in a constraint substitution looks it up in the
     * simple substitution
     *
     * @param v a logical variable
     */
    def lookup(v: Var) = s.lookup(v)
    override def constraints(v: Var) = c_lookup(v, c)
    /**
     * The length of a constraing substitution is the length of its simple substitution
     */
    def length: Int = s.length 
  }

  /**
   * <p>Uses an immutable map to store the substitution.<br>
   * If the computation is lookup-heavy, this should be faster.</p>
   *
   * <p>Not used by default as memory consumption is heavy -- palprod_o
   * causes heap OOM exception.</p>
   */
  case class MSubst(m: Map[Var, Any]) extends Subst {
    def extend(v: Var, x: Any) = Some(MSubst(m + (v -> x)))
    def lookup(v: Var) = m.get(v)
    def length = m.size
  }

  val empty_msubst = MSubst(Map())

//  import clojure.lang.IPersistentMap
//  import clojure.lang.PersistentHashMap
//
//  /**
//   * A substitution based on Clojure's PersistentHashMap
//   * (earlier based on Odersky's colleague's work at EPFL!)
//   *
//   * Requires a modified Clojure, because right now the
//   * MapEntry interface exposes a val() getter which clashes
//   * with the Scala keyword
//   */
//  case class CljSubst(m: IPersistentMap) extends Subst {
//    def extend(v: Var, x: Any) = Some(CljSubst(m.assoc(v, x)))
//    def lookup(v: Var) = {
//      val res = m.entryAt(v)
//      if (res != null) Some(res.`val`)
//      else None
//    }
//    def length = m.count
//  }
//
//  val empty_cljsubst = CljSubst(PersistentHashMap.EMPTY)
  import scala.collection.immutable.Map

  /**
    * A substitution based on Scala's immutable hashmap (which now looks like Clojure's PersistentHashMap)
    */
  case class CljSubst(m: Map[Any, Any]) extends Subst {
    def lookup(v: Var) = {
      m.get(v)
    }
    def extend(v: Var, x: Any) = Some(CljSubst(m + (v -> x)))
    def length = m.size
  }

  val empty_cljsubst = CljSubst(Map())
}

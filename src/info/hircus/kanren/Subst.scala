package info.hircus.kanren

import info.hircus.kanren.MiniKanren._

object Substitution {

  object EmptySubst extends Subst {
    def extend(v: Var, x: Any) = Some(SimpleSubst(v,x,this))
    def lookup(v: Var) = None
    def length: Int = 0
  }

  case class SimpleSubst(v: Var, x: Any, s: Subst) extends Subst {
    def extend(v: Var, x: Any) = Some(SimpleSubst(v,x,this))
    def lookup(v: Var) = if (this.v == v) Some(x) else s.lookup(v)
    def length: Int = 1 + s.length
  }

  abstract class ConstraintSubst extends Subst {
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
    def extend(v: Var, x: Any) =
      if (this.constraints(v) contains x) None
      else Some(ConstraintSubstN(SimpleSubst(v,x,this), c))

    override def c_extend(v: Var, x: Any) = ConstraintSubst0(c_insert(v,x,c))

    def lookup(v: Var) = None
    override def constraints(v: Var) = c_lookup(v, c)
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
  
    def lookup(v: Var) = s.lookup(v)
    override def constraints(v: Var) = c_lookup(v, c)
    def length: Int = 1 + s.length 
  }
}

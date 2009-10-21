package info.hircus.kanren

import info.hircus.kanren.MiniKanren.Var

trait Subst {
  def extend(v: Var, x: Any): Subst
  def lookup(v: Var): Option[Any]
  def constraints(v: Var): List[Any]
  def length: Int
}

object empty_s extends Subst {
  def extend(v: Var, x: Any) = SimpleSubst(v,x,this)
  def lookup(v: Var) = None
  def constraints(v: Var) = Nil
  def length: Int = 0
}

case class SimpleSubst(v: Var, x: Any, s: Subst) extends Subst {
  def extend(v: Var, x: Any) = SimpleSubst(v,x,this)
  def lookup(v: Var) = if (this.v == v) Some(x) else s.lookup(v)
  def constraints(v: Var) = Nil
  def length: Int = 1 + s.length
}


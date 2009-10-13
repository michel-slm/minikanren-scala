package info.hircus.kanren

object MKLib {
  import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.MKLogic._

  def car_o(p: Any, a: Any): Goal = {
    val d = make_var('d)
    mkEqual( (a, d), p ) _
  }

  def cdr_o(p: Any, d: Any): Goal = {
    val a = make_var('a)
    mkEqual( (a, d), p ) _
  }

  def pair_o(p: Any): Goal = {
    val a = make_var('a)
    val d = make_var('d)
    mkEqual( (a, d), p ) _
  }

  def null_o(x: Any): Goal = {
    mkEqual( Nil, x ) _
  }

  def list_o (l: Any): Goal =
    cond_e(List(null_o(l), succeed _),
           List(pair_o(l), { s: Subst =>
             val d = make_var('d)
             all(cdr_o(l, d),
                 list_o(d))(s) }))

  /* not sure why this is even needed */
  def eq_car_o(l: Any, x: Any): Goal =
    car_o(l, x)
  
  def member_o(x: Any, l: Any): Goal =
    cond_e(List(null_o(l), fail _),
           List(eq_car_o(l, x), succeed _),
           List({s: Subst =>
	     val d = make_var('d)
             all(cdr_o(l, d),
                 member_o(x, d))(s)
               } ))
}

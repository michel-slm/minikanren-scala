package info.hircus.kanren

object MKLib {
  import info.hircus.kanren.MiniKanren._
  import info.hircus.kanren.MKLogic._

  def car_o(p: Any, a: Any): Goal = {
    val d = make_var('d)
    mkEqual( (a, d), p )
  }

  def cdr_o(p: Any, d: Any): Goal = {
    val a = make_var('a)
    mkEqual( (a, d), p )
  }

  def pair_o(p: Any): Goal = {
    val a = make_var('a)
    val d = make_var('d)
    mkEqual( (a, d), p )
  }

  def null_o(x: Any): Goal = {
    mkEqual( Nil, x )
  }

  def list_o (l: Any): Goal =
    if_e(null_o(l), succeed,
	 if_e(pair_o(l), { s: Subst =>
           val d = make_var('d)
	   all(cdr_o(l, d),
               list_o(d))(s) },
	      fail))

  
  def member_o(x: Any, l: Any): Goal =
    if_e(null_o(l), fail,
         if_e(car_o(l, x), succeed,
	      {s: Subst =>
		val d = make_var('d)
                all(cdr_o(l, d),
                member_o(x, d))(s)
               } ))
}

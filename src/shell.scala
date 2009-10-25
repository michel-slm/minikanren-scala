import info.hircus.kanren.MiniKanren._
import info.hircus.kanren.Prelude._
import info.hircus.kanren.MKMath._
import info.hircus.kanren.examples.PalProd._
import info.hircus.kanren.examples.SendMoreMoney._

var x = make_var('x)
var y = make_var('y)
var z = make_var('z)

def time(block: => Any) = {
  val start = System currentTimeMillis ()
  val res   = block
  val stop  = System currentTimeMillis ()
  ((stop-start), res)
}

def ntimes(n: Int, block: => Any) = {
  // folding a list of Longs is cumbersome
  def adder(x:Long,y:Long) = x+y
  val zero : Long = 0

  // compute only once!
  val res = (for (i <- 0 until n) yield (time(block) _1)).toList
  println("Elapsed times: " + res)
  println("Avg: " + (res.foldLeft(zero)(adder) / n))
}

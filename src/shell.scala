import info.hircus.kanren.MiniKanren._
import info.hircus.kanren.Prelude._
import info.hircus.kanren.MKMath._
import info.hircus.kanren.examples.PalProd._

var x = make_var('x)
var y = make_var('y)
var z = make_var('z)

def time(block: => Any) = {
  val start = System currentTimeMillis ()
  val res   = block
  val stop  = System currentTimeMillis ()
  println("Elapsed: " + (stop-start) + " ms")
  res
}

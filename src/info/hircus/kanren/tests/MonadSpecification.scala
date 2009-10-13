package info.hircus.kanren.tests

import org.scalacheck._
import info.hircus.kanren.MiniKanren._
import info.hircus.kanren.MKLogic._
import info.hircus.kanren.MKLib._

object MonadSpecification extends Properties("Monad") {
  import Prop.forAll

  val v = make_var('v)
  val w = make_var('w)

}

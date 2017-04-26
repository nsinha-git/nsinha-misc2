package com.nsinha.common

/** Created by nsinha on 4/14/17.
  */

object StandardBooleanCoercions {
  implicit def intToBool(x : Int) : Boolean = {
    if (x == 0) false else true
  }

  implicit def longToBool(x : Long) : Boolean = {
    if (x == 0) false else true
  }

  implicit def stringToBool(x : String) : Boolean = {
    if (x.size == 0) false else true
  }

  implicit def nullToFalse[A](x : A) : Boolean = {
    if (x == null) return false
    true
  }

}

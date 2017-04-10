package com.nsinha.common

/** Created by nsinha on 4/9/17.
  */
object MapUtils {

  def invertMap[A, B](mp : Map[A, B]) : Map[B, A] = {
    val res = mp.foldLeft(Map[B, A] ()) { (Z, el) ⇒
      Z + (el._2 → el._1)
    }
    res
  }

}

package com.nsinha.common

import scala.collection.mutable

/** Created by nsinha on 4/21/17.
  */

class CircularQueue[A](sz : Int = 100) {
  val innerQue = {
    val hashMap = new mutable.HashMap[Int, A]()
    hashMap
  }
  var nextWrite : Int = 0
  var queSz : Int = 0

  def insert(x : A) : CircularQueue[A] = {
    innerQue(nextWrite) = x
    nextWrite = (nextWrite + 1) % sz
    queSz = if (queSz != sz) queSz + 1 else queSz
    this
  }

  def get : Option[A] = {
    if (queSz > 0) {
      val read = (nextWrite - 1) % sz
      val res = innerQue(read)
      queSz = queSz - 1
      nextWrite = (nextWrite - 1) % sz
      Option(res)
    }
    else {
      None
    }
  }

  def size : Int = {
    queSz
  }

}

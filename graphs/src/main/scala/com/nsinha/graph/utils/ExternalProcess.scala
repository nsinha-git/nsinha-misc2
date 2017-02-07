package com.nsinha.graph.utils

import scala.sys.process._

/** Created by nsinha on 1/31/17.
  */
object ExternalProcess {

  def apply(cmd : Seq[String]) = {
    val run = cmd.!
    println(s"run: $run")
  }

}

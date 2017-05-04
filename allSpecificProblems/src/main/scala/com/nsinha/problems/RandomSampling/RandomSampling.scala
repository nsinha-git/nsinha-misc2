package com.nsinha.problems.RandomSampling

import org.scalatest.FunSuite

import scala.util.Random

/** Created by nsinha on 4/26/17.
  */
object RandomSampling {

  def choose(k : Int) : Boolean = {
    //run a 1/k sampler
    Random.setSeed(System.nanoTime())
    Random.nextInt(k) == 0

  }

}

class RandomSamplingTesting extends FunSuite {

  test("a") {
    Range(0, 10) map { x ⇒
      var cnt = 0
      for (i ← Range(1, 101)) {
        if (RandomSampling.choose(i)) cnt = cnt + 1
      }
      println(cnt)
    }

  }

}

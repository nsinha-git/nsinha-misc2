package com.nsinha.library

/**
  * Created by nsinha on 2/2/17.
  */
trait MonadicResult[A,B,C] {
  type StateUpdateFn  =  C => B
  val state: B
  val result: A
  val updateState:  StateUpdateFn
  def apply(newResult:A, n: C): MonadicResult[A,B,C]
  def map(f: A => A ): MonadicResult[A,B,C]
  def flatMap(f: A => MonadicResult[A,B,C]): MonadicResult[A,B,C]
}

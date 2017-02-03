package com.nsinha.library

/**
  * Created by nsinha on 2/2/17.
  */

trait  MonoidType[A] {
  val value: A
  def zero: A
  def getValue: A
}
trait MonadicResult[A,B] {
  val state: MonoidType[B]
  val result: MonoidType[A]
  def map(f: (A,B) => (A,B) ): MonadicResult[A,B]
  def flatMap(f: (A,B) => MonadicResult[A,B]): MonadicResult[A,B]
  def getResult: A = result.getValue
  def getState: B = state.getValue
  def zero(): MonadicResult[A,B]
}


class MonadicResultImpl[A,B](_result: A, _state: B)(implicit fA: () => A, fB: () => B) extends MonadicResult[A,B] {
  val state = new MonoidType[B] {
    override val value = _state
    override def zero = fB()
    override def getValue = value
  }
  val result = new MonoidType[A] {
    override val value = _result
    override def zero = fA()
    override def getValue = value
  }
  override def zero(): MonadicResult[A,B] = new MonadicResultImpl[A,B](fA(),fB())

  override def map(f: (A,B) => (A,B)): MonadicResult[A,B] = {
    val x = f(result.getValue, state.getValue)
    new MonadicResultImpl[A,B](x._1,x._2)
  }

  override def flatMap(f: (A,B) => MonadicResult[A, B]) = {
    f(result.getValue, state.getValue)
  }

}


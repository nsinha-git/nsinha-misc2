package com.nsinha.graph.interfaces.Common

/**
  * Created by nsinha on 2/13/17.
  */

trait GroupElem[A] {
  def `+`(that: A): A
  def `0`: A
}

trait RingElem[A] extends GroupElem[A] {
  def `*`(that: A): A
  def `1`: A
}

case class UnAssociativeGroupElem(name: String) extends Ordered[UnAssociativeGroupElem] with GroupElem[UnAssociativeGroupElem] {
  override def compare(that : UnAssociativeGroupElem) : Int = name.compare(that.name)
  override def `+`(that: UnAssociativeGroupElem) = UnAssociativeGroupElem((this.name.toDouble - that.name.toDouble).toString)
  override def `0` = UnAssociativeGroupElem("0")
}

case class AssociativeGroupElem(name: String) extends Ordered[AssociativeGroupElem] with GroupElem[AssociativeGroupElem] {
  override def compare(that : AssociativeGroupElem) : Int = name.compare(that.name)
  override def `+`(that: AssociativeGroupElem) = AssociativeGroupElem((this.name.toDouble + that.name.toDouble).toString)
  override def `0` = AssociativeGroupElem("0")
}

case class UnAssociativeNonDistributiveRingElem(name: String) extends Ordered[UnAssociativeNonDistributiveRingElem] with RingElem[UnAssociativeNonDistributiveRingElem] {
  override def compare(that : UnAssociativeNonDistributiveRingElem) : Int = name.compare(that.name)
  override def `+`(that: UnAssociativeNonDistributiveRingElem) = UnAssociativeNonDistributiveRingElem((this.name.toDouble - that.name.toDouble).toString)
  override def `0` = UnAssociativeNonDistributiveRingElem("0")
  override def `*`(that: UnAssociativeNonDistributiveRingElem) = UnAssociativeNonDistributiveRingElem((this.name.toDouble / that.name.toDouble).toString)
  override def `1` = UnAssociativeNonDistributiveRingElem("1")
}

case class AssociativeNonDistributiveRingElem(name: String) extends Ordered[AssociativeNonDistributiveRingElem] with RingElem[AssociativeNonDistributiveRingElem] {
  override def compare(that : AssociativeNonDistributiveRingElem) : Int = name.compare(that.name)
  override def `+`(that: AssociativeNonDistributiveRingElem) = AssociativeNonDistributiveRingElem((this.name.toDouble + that.name.toDouble).toString)
  override def `0` = AssociativeNonDistributiveRingElem("0")
  override def `*`(that: AssociativeNonDistributiveRingElem) = AssociativeNonDistributiveRingElem((this.name.toDouble / that.name.toDouble).toString)
  override def `1` = AssociativeNonDistributiveRingElem("1")
}

case class AssociativeDistributiveRingElem(name: String) extends Ordered[AssociativeDistributiveRingElem] with RingElem[AssociativeDistributiveRingElem] {
  override def compare(that : AssociativeDistributiveRingElem) : Int = name.compare(that.name)
  override def `+`(that: AssociativeDistributiveRingElem) = AssociativeDistributiveRingElem((this.name.toDouble + that.name.toDouble).toString)
  override def `0` = AssociativeDistributiveRingElem("0")
  override def `*`(that: AssociativeDistributiveRingElem) = AssociativeDistributiveRingElem((this.name.toDouble * that.name.toDouble).toString)
  override def `1` = AssociativeDistributiveRingElem("1")
}
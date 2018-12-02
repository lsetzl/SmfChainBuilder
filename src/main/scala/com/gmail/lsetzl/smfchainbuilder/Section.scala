package com.gmail.lsetzl.smfchainbuilder

case class Section(start: Int, duration: Int) {
  val end: Int = start + duration

  def extend(a: Int): Section = copy(duration = duration + a)
}

object Section {
  def apply(start: Double, duration: Double): Section = {
    def doubleToTick(value: Double): Int = (value * SmfChainBuilder.RESOLUTION + (value % 1.0) * 1000).toInt

    Section(doubleToTick(start), doubleToTick(duration))
  }

  val all: Section = Section(0, Int.MaxValue)
}
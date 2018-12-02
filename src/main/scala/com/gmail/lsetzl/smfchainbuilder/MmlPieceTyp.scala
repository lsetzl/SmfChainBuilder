package com.gmail.lsetzl.smfchainbuilder

sealed abstract class MmlPieceTyp(val rule: String)

object MmlPieceTyp {
  val values: Seq[MmlPieceTyp] = List(Note, Rest, BaseLength, Extend)

  def fromCommand(command: Char): Option[MmlPieceTyp] = values.find(v => command.toString.matches(v.rule))

  case object Note extends MmlPieceTyp("[a-g]")

  case object Rest extends MmlPieceTyp("r")

  case object BaseLength extends MmlPieceTyp(":")

  case object Extend extends MmlPieceTyp("\\.")
}

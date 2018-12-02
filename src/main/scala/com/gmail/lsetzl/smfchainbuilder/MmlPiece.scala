package com.gmail.lsetzl.smfchainbuilder

case class MmlPiece(command: Char, options: Seq[Char], value: Int) {
  val typ: Option[MmlPieceTyp] = MmlPieceTyp.fromCommand(command)

  val key: Int = "c d ef g a b".indexOf(command)

  val octave: Int = options.count(_ == '^') - options.count(_ == '_')

  val shift: Int = options.count(_ == '+') - options.count(_ == '-')

  val isBaseKey: Boolean = options.contains('@')
}

object MmlPiece {
  def apply(source: String): MmlPiece = {
    val command = source.head
    val options = source.tail.takeWhile(!_.isDigit)
    val value = {
      val v = source.drop(options.length + 1)
      if (v == "") 1 else v.toInt
    }
    MmlPiece(command, options, value)
  }
}

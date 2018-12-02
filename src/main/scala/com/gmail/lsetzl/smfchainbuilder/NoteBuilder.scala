package com.gmail.lsetzl.smfchainbuilder

case class NoteBuilder(channelNumber: Int, trackNumber: Int, tick: Int, baseDuration: Int = 12, baseKey: Int = 0, notes: Seq[Note] = Nil) {
  def +(piece: MmlPiece): NoteBuilder = {
    val duration = baseDuration * piece.value
    piece.typ.orNull match {
      case null =>
        sys.error(s"Illegal mml command: ${piece.command}")
      case MmlPieceTyp.Rest =>
        forwardTick(duration)
      case MmlPieceTyp.BaseLength =>
        copy(baseDuration = piece.value)
      case MmlPieceTyp.Extend =>
        extendLastNote(duration).forwardTick(duration)
      case MmlPieceTyp.Note =>
        val note = Note(channelNumber, trackNumber, Section(tick, duration), piece.key, 0, piece.octave, piece.shift, 0, 1)
        forwardTick(duration)
          .copy(notes = notes :+ note)
          .copy(baseKey = if (piece.isBaseKey) piece.key else baseKey)
    }
  }

  def forwardTick(value: Int): NoteBuilder = copy(tick = tick + value)

  def extendLastNote(duration: Int): NoteBuilder = copy(notes = notes.init :+ notes.last.extend(duration))

  def addedOctaveIfUnderBase: NoteBuilder = copy(notes = notes.map(_.addedOctaveIfUnderBase(baseKey)))
}

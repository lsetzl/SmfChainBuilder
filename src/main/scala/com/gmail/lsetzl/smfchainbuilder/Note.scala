package com.gmail.lsetzl.smfchainbuilder
import javax.sound.midi.{MidiEvent, ShortMessage}

case class Note(channelNumber: Int, trackNumber: Int, section: Section, key: Int, velocity: Int, octave: Int, shift: Int, delay: Int, cut: Int) extends Command {
  override val level: CommandLevel = CommandLevel.Track

  override def midiEvents: Seq[MidiEvent] = {
    List(
      new MidiEvent(new ShortMessage(ShortMessage.NOTE_ON, channelNumber, key, velocity), section.start + delay),
      new MidiEvent(new ShortMessage(ShortMessage.NOTE_OFF, channelNumber, key, 0), section.end - cut)
    )
  }

  def extend(a: Int): Note = copy(section = section.extend(a))

  def addedOctaveIfUnderBase(base: Int): Note = copy(octave = octave + (if (key < base) 1 else 0))
}

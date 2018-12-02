package com.gmail.lsetzl.smfchainbuilder

import javax.sound.midi.MidiEvent

sealed trait CommandLevel
object CommandLevel {
  object Song extends CommandLevel
  object Channel extends CommandLevel
  object Track extends CommandLevel
}

trait Command {
  val level: CommandLevel
  val channelNumber: Int
  val trackNumber: Int
  val section: Section

  def midiEvents: Seq[MidiEvent]

  def isSong: Boolean = level == CommandLevel.Song

  def isChannel: Boolean = level == CommandLevel.Channel

  def isTrack: Boolean = level == CommandLevel.Track
}

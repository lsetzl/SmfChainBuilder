package com.gmail.lsetzl.smfchainbuilder

import java.io.File

import javax.sound.midi._

case class SmfChainBuilder(commands: Seq[Command], channelNumber: Int, trackNumber: Int, section: Section) {
  def channelNumbers = commands.filter(_.isChannel).map(_.channelNumber).distinct.filter(_ != 0)

  def forwardChannel: SmfChainBuilder = copy(channelNumber = channelNumber + 1, trackNumber = 0)

  def forwardTrack: SmfChainBuilder = copy(trackNumber = trackNumber + 1)

  def section(a: Section): SmfChainBuilder = copy(section = a)

  def notes(mmls: String*): SmfChainBuilder = {
    val noteBuilder = mmls.mkString
      .replaceAll("\\s", "")
      .replaceAll("[a-gr:\\.]", "\t$0")
      .trim
      .split("\t")
      .map(MmlPiece(_))
      .foldLeft(NoteBuilder(channelNumber, trackNumber, section.start)) { (s, p) => s + p }
    assert(section.end != Int.MaxValue && noteBuilder.tick != section.end,
      s"notes area error expected=${section.end}, actual=${noteBuilder.tick}, mmls=$mmls")

    add(noteBuilder.addedOctaveIfUnderBase.notes)
  }

  def add(a: Seq[Command]): SmfChainBuilder = copy(commands = commands ++ a)

  def write(path: String): Unit = {
    val sequence = new Sequence(Sequence.PPQ, SmfChainBuilder.RESOLUTION)

    def addTrack(cs: Seq[Command]): Unit = {
      val track: Track = sequence.createTrack()
      cs.flatMap(_.midiEvents).foreach(track.add)
    }

    addTrack(commands.filter(_.isSong))

    channelNumbers.foreach { cn =>
      val inChannelCommands = {
        commands.filter(c => (c.isChannel || c.isTrack) && Seq(cn, 0).contains(c.channelNumber))
      }
      addTrack(inChannelCommands.filter(_.isChannel))

      val inChannelTrackCommands = inChannelCommands.filter(_.isTrack)
      val trackNumbers = inChannelTrackCommands.map(_.trackNumber).distinct.filter(_ != 0)
      trackNumbers.foreach(tn => addTrack(inChannelTrackCommands.filter(_.trackNumber == tn)))
    }
    MidiSystem.write(sequence, 1, new File(path))
  }
}

object SmfChainBuilder {
  val RESOLUTION: Int = 48

  def apply(): BuilderWrapper.Song = {
    BuilderWrapper.Song(SmfChainBuilder(Nil, 0, 0, Section.all))
  }
}




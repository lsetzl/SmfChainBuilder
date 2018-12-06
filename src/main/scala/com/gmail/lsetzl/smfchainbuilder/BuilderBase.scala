package com.gmail.lsetzl.smfchainbuilder

trait BuilderBase {
  val song: Song
  val channels: Seq[Channel]
  val tracks: Seq[Track]
  val sections: Seq[Section]
}

case class Song(commands: Seq[Command[Song]], sectionNumber: SectionNumber = SectionNumber.All)

case class Channel(commands: Seq[Command[Channel]], sectionNumber: SectionNumber = SectionNumber.All)

case class Track(commands: Seq[Command[Track]], sectionNumber: SectionNumber = SectionNumber.All)
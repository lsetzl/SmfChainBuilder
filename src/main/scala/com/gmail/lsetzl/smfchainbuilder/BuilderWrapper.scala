package com.gmail.lsetzl.smfchainbuilder

sealed trait BuilderWrapper {
  val builder: SmfChainBuilder
}
object BuilderWrapper {
  case class Song(override val builder: SmfChainBuilder) extends BuilderWrapper {
    def channel: Channel = Channel(builder.forwardChannel)

    def section(a: Section): Song = Song(builder = builder.section(a))
  }

  case class Channel(override val builder: SmfChainBuilder) extends BuilderWrapper {
    def channel: Channel = Channel(builder.forwardChannel)

    def track: Track = Track(builder.forwardChannel)

    def section(a: Section): Channel = Channel(builder = builder.section(a))
  }

  case class Track(override val builder: SmfChainBuilder) extends BuilderWrapper {
    def channel: Channel = Channel(builder.forwardChannel)

    def track: Track = Track(builder.forwardChannel)

    def section(a: Section): Track = Track(builder = builder.section(a))

    def notes(mml: String): Track = Track(builder = builder.notes(mml))

    def write(path: String): Unit = builder.write(path)
  }

}

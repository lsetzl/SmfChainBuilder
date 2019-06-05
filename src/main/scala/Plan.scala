import com.gmail.lsetzl.simplemidiwriterwrapper.{MidiEvent, Track => MidiTrack}

package object Plan {
  val Resolution: Int = 48
  val TicksInBar: Int = Resolution * 4
}

trait SmfChainBuilderApp extends App {

  implicit class ExtendedInt(value: Int) {
    def ~(end: Int): Values = Values(value, end)
  }

  implicit def IntToValues(value: Int): Values = Values(value, value)

  implicit class ExtendedDouble(value: Double) {
    def ~(end: Double): Section = Section(value, end)
  }

  implicit def DoubleToSection(value: Double): Section = Section(value, value)

  def song: Builder.Song = Builder.Song(BuilderBody())
}

case class Track(channelNumber: ChannelNumber, commands: Seq[Command]) {
  def +(a: Seq[Command]): Track = copy(commands = commands ++ a)
}

object Track {
  def apply(channelNumber: ChannelNumber): Track = Track(channelNumber, Nil)
}

case class BuilderBody(tracks: Seq[Track], section: Section) {
  def updateLastTrack(f: Track => Track): BuilderBody = copy(tracks = tracks.updated(tracks.length - 1, f(tracks.last)))

  def +(a: Seq[Command]): BuilderBody = updateLastTrack(_ + a)

  def section(a: Section): BuilderBody = copy(section = section)

  def write(): Unit = ???
}

object BuilderBody {
  def apply(): BuilderBody = BuilderBody(Nil, Section(Tick.Zero, Tick(4)))
}

sealed trait Builder {
  type T

  protected val body: BuilderBody

  protected def update(a: BuilderBody): T

  def on(a: Section): T = update(body.section(a))

  protected def +(a: Command*): T = update(body + a)
}

object Builder {

  case class Song(override val body: BuilderBody) extends Builder {
    override type T = Song

    override def update(a: BuilderBody): T = Song(a)

    def masterVolume(values: Values): Song = add(Command.MasterVolume(body.section, values))

    def channel: Channel = Channel(body)
  }

  case class Channel(override val body: BuilderBody) extends Builder {
    override type T = Channel

    override def update(a: BuilderBody): T = Channel(a)

    def track: Track = Track(body)

    def expression(values: Values): Channel = add(Command.Expression(body.channelNumber, body.section, values))
  }

  case class Track(override val body: BuilderBody) extends Builder {
    override type T = Track

    override def update(a: BuilderBody): T = Track(a)

    def channel: Channel = Channel(body)

    def notes(mmls: String*): Track = add(NoteBuilder.build(mmls.mkString))

    def write(path: String): Unit = ???
  }

}

case class ChannelNumber(value: Int) {
  require(value >= 0)
}

object ChannelNumber {
  val All: ChannelNumber = ChannelNumber(0)
}

case class TrackNumber(value: Int) {
  require(value >= 0)
}

object TrackNumber {
  val All: TrackNumber = TrackNumber(0)
}

case class Tick(value: Int) {
  require(value >= 0)
}

object Tick {
  val Zero: Tick = Tick(0)
  val Max: Tick = Tick(Int.MaxValue)
}

case class Section(start: Tick, end: Tick) {

}

object Section {
  val All: Section = Section(Tick.Zero, Tick.Max)

  def apply(start: Int, end: Int): Section = Section(Tick(start), Tick(end))

  def apply(startBar: Double, endBar: Double): Section = {
    Section((startBar * Plan.TicksInBar).toInt, (endBar * Plan.TicksInBar).toInt)
  }
}

object NoteBuilder {
  def build(mml: String): Seq[Command.Note] = ???
}

sealed trait Command {
  val channelNumber: ChannelNumber = ChannelNumber.All
  val trackNumber: TrackNumber = TrackNumber.All
  val section: Section
  val values: Values
}

object Command {
  case class MasterVolume(override val section: Section, override val values: Values) extends Command

  case class Expression(override val channelNumber: ChannelNumber,
                        override val section: Section, override val values: Values) extends Command

  case class Note(override val channelNumber: ChannelNumber, override val trackNumber: TrackNumber,
                  override val section: Section, override val values: Values) extends Command
}

case class Values(start: Int, end: Int)


object Test extends SmfChainBuilderApp {
  val p = 0.0 ~ 8.0
  val pa = 9.0 ~ 16.0

  song
    .on(p).masterVolume(0 ~ 127)

    .channel
    .expression(127)
    .track
    .on(p).notes("c..c..c.c..b..a./a6g.g8/a..a..a.g.a.g.d./e4a4b4c4", "e6d.d8/e6d.d8/a..b..c.a..b..c./c4b4a4b4")
    .on(pa).notes("b6c.c8/.16/.16/.16")

    .channel
    .track
    .on(p).notes("f8g8/a16/f8g8/a8g8/f16/e16/d16/g4g4g4g4")

    .write("test.mid")
}

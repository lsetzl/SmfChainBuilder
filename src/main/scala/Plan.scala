import com.gmail.lsetzl.simplemidiwriterwrapper.{MidiEvent, Track => MidiTrack}

package object Plan {
  val Resolution: Int = 48
  val TicksInBar: Int = Resolution * 4
}

trait SmfChainBuilderApp extends App {

  implicit class ExtendedInt(a: Int) {
    def ~(b: Int): Parameters = Parameters(a, b)
  }

  implicit def IntToParameters(a: Int): Parameters = Parameters(a, a)

  implicit class ExtendedDouble(a: Double) {
    def ~(b: Double): Parameters = Parameters(a, b)
  }

  implicit def DoubleToParameters(a: Double): Parameters = Parameters(a, a)

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

  protected def add(a: Command): T = add(List(a))

  protected def add(a: Seq[Command]): T = update(body + a)

  def write(path: String): Unit = ???
}

object Builder {

  case class Song(override val body: BuilderBody) extends Builder {
    override type T = Song

    override def update(a: BuilderBody): T = Song(a)

    def channel: Channel = Channel(body)

    def tempo(parameters: Parameters): Song = add(Command.Tempo(body.section, parameters.toValues))

    def volume(parameters: Parameters): Song = add(Command.ControlChange(body.section, 7, parameters.toPercents))
  }

  case class Channel(override val body: BuilderBody) extends Builder {
    override type T = Channel

    override def update(a: BuilderBody): T = Channel(a)

    def track: Track = Track(body)

    def expression(values: Values): Channel = add(Command.Expression(body.section, values))
  }

  case class Track(override val body: BuilderBody) extends Builder {
    override type T = Track

    override def update(a: BuilderBody): T = Track(a)

    def channel: Channel = Channel(body)

    def notes(mmls: String*): Track = add(NoteBuilder.build(mmls.mkString))
  }

}

case class ChannelNumber(value: Int) {
  require(value >= 0)
}

object ChannelNumber {
  val All: ChannelNumber = ChannelNumber(0)
}

case class Tick(value: Int) {
  require(value >= 0)
}

object Tick {
  val Zero: Tick = Tick(0)
  val Max: Tick = Tick(Int.MaxValue)
}

object NoteBuilder {
  def build(mml: String): Seq[Command.Note] = ???
}

sealed trait Command {
  val section: Section
}

object Command {
  case class Tempo(override val section: Section, values: Values) extends Command

  case class Msb(override val section: Section, value: Int) extends Command

  case class Modulation(override val section: Section, percents: Percents) extends Command

  case class Volume(override val section: Section, percents: Percents) extends Command

  case class Expression(override val section: Section, percents: Percents) extends Command

  case class Volume(override val section: Section, percents: Percents) extends Command

  case class Note(override val section: Section, key: Int) extends Command
}

case class Parameters(start: Double, end: Double) {
  def toValues: Values = Values(start.toInt, end.toInt)

  def toPercents: Percents = Percents(start, end)

  def toSections: Section = {
    Section(Tick(start.toInt * 48 + (start * 100).toInt % 100), Tick(end.toInt * 48 + (end * 100).toInt % 100))
  }
}

case class Values(start: Int, end: Int)

case class Percents(start: Double, end: Double)

case class Section(start: Tick, end: Tick)

object Section {
  val All: Section = Section(Tick.Zero, Tick.Max)
}

object Test extends SmfChainBuilderApp {
  val p = 0.0 ~ 8.0
  val pa = 9.0 ~ 16.0

  song
    .on(p).volume(0 ~ 127)

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

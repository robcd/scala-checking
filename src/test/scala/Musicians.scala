import io.Source
import org.lafros.scala.Checking
/**
 *
 */
trait Musicians extends Checking {
  type R = String

  object Genre extends Enumeration {
    type Genre = Value
    val easyListening = Value("easy listening")
    val jazz, pop = Value
  }
  import Genre._

  object Instrument extends Enumeration {
    type Instrument = Value
    val panPipes = Value("pan pipes")
    val sax, trumpet, voice = Value
  }
  import Instrument._

  case class Musician(       name: String,
                            genre: Genre,
                      instruments: Iterable[Instrument])

  object Musician {
    object checks {
      def name(s: String) = if (s.length > 0) Okay(s) else Reason("empty name")
      def genre(s: String) = try {
        Genre.withName(s.trim) match {
          case `easyListening` => Reason(s)
          case other => Okay(other)
        }
      } catch {
        case ex: NoSuchElementException => Reason("unrecognised genre: "+ s)
      }
      def instruments(ss: Iterable[String]) = try {
        val instruments = for (s <- ss) yield Instrument.withName(s.trim)
        if (instruments exists(_ == panPipes)) Reason("includes "+ panPipes)
        else Okay(instruments)
      }
      catch {
        case ex: NoSuchElementException => Reason("unrecognised instrument: "+ ex.getMessage)
      }
    }
  }

  def readFromFile(fname: String): Iterable[Checked[Musician, Iterable[String]]] = for {
    line <- Source.fromFile(fname).getLines.toIterable
    tokens = line.split("\\,")
  } yield {
    import Musician._
    import checks._
    fs((apply _).curried) <*> name(tokens(0)) <*>
                              genre(tokens(1)) <*>
                              instruments(tokens.drop(2))
  }
}

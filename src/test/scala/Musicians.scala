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
    val piano, sax, trumpet, voice = Value
  }
  import Instrument._

  case class Musician(       name: String,
                            genre: Genre,
                      instruments: Set[Instrument])

  object Musician {
    object checks {
      def name(s: String) = if (s.length > 0) s toOkay else "empty name" toReason
      def genre(s: String) = try {
        Genre.withName(s) match {
          case `easyListening` => s toReason
          case other => other toOkay
        }
      } catch {
        case ex: NoSuchElementException => Reason("unrecognised genre: "+ s)
      }
      def instruments(ss: Iterable[String]) = try {
        val instruments = for (_s <- ss; s = _s.trim) yield try {
          Instrument.withName(s)
        } catch {
          case ex: NoSuchElementException =>
            throw new NoSuchElementException("unrecognised instrument: "+ s)
        }
        if (instruments exists(_ == panPipes)) Reason("plays the "+ panPipes)
        else instruments.toSet.toOkay
      }
      catch {
        case ex: NoSuchElementException => ex.getMessage toReason
      }
    }
  }

  def readFromFile(fname: String): Iterable[Checked[Musician, Iterable[String]]] = for {
    line <- Source.fromFile(fname).getLines.toIterable
    tokens = line.split("\\,")
  } yield {
    import Musician._
    import checks._
    fs((apply _).curried) <*> name(tokens(0).trim) <*>
                              genre(tokens(1).trim) <*>
                              instruments(tokens.drop(2))
  }
}

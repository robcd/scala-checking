import org.lafros.scala.Checking
/**
 *
 */
trait Musicians extends Checking {
  object Genre extends Enumeration {
    type Genre = Value
    val easyListening, jazz, pop = Value
  }
  import Genre._

  object Instrument extends Enumeration {
    type Instrument = Value
    val panPipes, sax, trumpet, voice = Value
  }
  import Instrument._

  case class Musician(   surname: String,
                       firstname: String,
                           genre: Genre,
                      instrument: Instrument)

  def readFromFile(name: String): (Iterable[Musician], Iterable[String]) = {
    null
  }
}

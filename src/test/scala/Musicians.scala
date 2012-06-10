/*
 * Copyright 2012 Latterfrosken Software Development Limited
 *
 * Licensed under the Apache License, Version 2.0 (the "License");
 * you may not use this file except in compliance with the License.
 * You may obtain a copy of the License at
 *
 *     http://www.apache.org/licenses/LICENSE-2.0
 *
 * Unless required by applicable law or agreed to in writing, software
 * distributed under the License is distributed on an "AS IS" BASIS,
 * WITHOUT WARRANTIES OR CONDITIONS OF ANY KIND, either express or implied.
 * See the License for the specific language governing permissions and
 * limitations under the License.
 */
import io.Source
import org.lafros.scala.Checking
/**
 * provides readFromFile method for converting each line of musicians.db (in project's root dir)
 * into a Musician, with the aid of Checking.
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

  // return type is Iterable[Checked[Musician, Iterable[String]]]
  def readFromFile(fname: String) = for {
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

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
object printMusicians extends App with Musicians {
  val checkedMusos = readFromFile("musicians.db") // Iterable[Checked[Musician, R]]

  checkedMusos foreach println

  // The following input...
  //
  //   Stevie Wonder,            pop, voice, piano
  //     Chick Corea,           jazz, piano
  // Michael Brecker,           jazz, sax
  //   Randy Brecker,           jazz, trumpet
  //  Charlie Parker,           jazz, sax
  //       Thor Chuh, easy listening, pan pipes
  //    Des O'Connor, easy listening, voice
  //     Rolf Harris, easy listening, voice, didgeridoo
  //
  // ...produces the following output:
  //
  // Okay(Musician(Stevie Wonder,pop,ArrayBuffer(voice, piano)))
  // Okay(Musician(Chick Corea,jazz,ArrayBuffer(piano)))
  // Okay(Musician(Michael Brecker,jazz,ArrayBuffer(sax)))
  // Okay(Musician(Randy Brecker,jazz,ArrayBuffer(trumpet)))
  // Okay(Musician(Charlie Parker,jazz,ArrayBuffer(sax)))
  // Reason(List(easy listening, plays the pan pipes))
  // Reason(List(easy listening))
  // Reason(List(easy listening, unrecognised instrument: didgeridoo))

  val okayMusos = for {
    checkedMuso <- checkedMusos
    if checkedMuso.isOkay
    okayMuso = checkedMuso.get
  } yield okayMuso

  println; okayMusos foreach println

  // Musician(Stevie Wonder,pop,ArrayBuffer(voice, piano))
  // Musician(Chick Corea,jazz,ArrayBuffer(piano))
  // Musician(Michael Brecker,jazz,ArrayBuffer(sax))
  // Musician(Randy Brecker,jazz,ArrayBuffer(trumpet))
  // Musician(Charlie Parker,jazz,ArrayBuffer(sax))

  def toText(muso: Musician) = {
    import muso._
    if (instruments(Instrument.voice)) {
      val realInstuments = instruments - Instrument.voice
      if (realInstuments isEmpty) name +" is a "+ genre +" singer."
      else name +" plays "+ genre +" "+ realInstuments.mkString(", ") +" and sings."
    }
    else
      name +" plays "+ genre +" "+ instruments.mkString(", ") +"."
  }

  // To convert an Iterable[A] to an Iterable[B] requires an
  // A => B
  def toCheckedText(chMuso: Checked[Musician, Iterable[String]]) = chMuso match {
    case Okay(muso) => Okay(toText(muso))
    case reason => reason
  }

  val checkedFormattedMusos = checkedMusos map toCheckedText

  println; checkedFormattedMusos foreach println

  // or simply
  val checkedFormattedMusos2 = checkedMusos map { _ map toText }

  println; checkedFormattedMusos2 foreach println

  // Okay(Stevie Wonder plays pop piano and sings.)
  // Okay(Chick Corea plays jazz piano.)
  // Okay(Michael Brecker plays jazz sax.)
  // Okay(Randy Brecker plays jazz trumpet.)
  // Okay(Charlie Parker plays jazz sax.)
  // Reason(List(easy listening, plays the pan pipes))
  // Reason(List(easy listening))
  // Reason(List(easy listening, unrecognised instrument: didgeridoo))
}

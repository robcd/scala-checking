object testMusicians extends App with Musicians {
  readFromFile("musicians.db") foreach println

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
// Reason(List(easy listening, includes pan pipes))
// Reason(List(easy listening))
// Reason(List(easy listening, unrecognised instrument: didgeridoo))
}

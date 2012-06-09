object testMusicians extends App with Musicians {
  val (musicians, errors) = readFromFile("musicians.db")
  musicians foreach println
  errors foreach println
}

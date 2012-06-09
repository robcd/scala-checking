object testMusicians extends App with Musicians {
  readFromFile("musicians.db") foreach println
}

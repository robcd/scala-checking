package org.lafros.scala

sealed abstract class Checked[+A, +S] {
  def isOkay: Boolean
  def map[B](f: A => B): Checked[B, S] = this match {
    case Okay(a) => Okay(f(a))
    case Kayo(b) => Kayo(b)
    case OkayAsKayo(Okay(a)) => OkayAsKayo(Okay(f(a)))
  }
  def flatMap[B, T >: S](f: A => Checked[B, T]): Checked[B, T] = this match {
    case Okay(a) => f(a)
    case Kayo(b) => Kayo(b)
    case OkayAsKayo(Okay(a)) => f(a) match {
      case okay @ Okay(_) => OkayAsKayo(okay)
      case kayo @ Kayo(_) => kayo
      case okayAsKayo @ OkayAsKayo(_) => okayAsKayo
    }
  }
  def foreach[B](f: A => B): Unit = this match {
    case Okay(a) => f(a)
    case Kayo(_) =>
    case OkayAsKayo(_) =>
  }
}

final case class Okay[+A, +S](a: A) extends Checked[A, S] {
  def isOkay = true
}

// final case class KayoAsOkay[+A, +S](kayo: Kayo[A, S]) extends Checked[A, S] {
//   def okay = true
// }

final case class Kayo[+O, +K](k: K) extends Checked[O, K] {
  def isOkay = false
}

final case class OkayAsKayo[+A, +S](okay: Okay[A, S]) extends Checked[A, S] {
  def isOkay = false
}

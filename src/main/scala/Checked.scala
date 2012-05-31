package org.lafros.scala

sealed abstract class Checked[+A, +R] {
  def isOkay: Boolean
  def get: A = this match {
    case Okay(a) => a
    case Kayo(_) => throw new NoSuchElementException("Kayo.get")
    case Checked.None(_) => throw new NoSuchElementException("None.get")
  }
  def reason: R = this match {
    case Okay(_) => throw new NoSuchElementException("Okay.reason")
    case Kayo(r) => r
    case Checked.None(_) => throw new NoSuchElementException("None.reason")
  }
  def getOrElse[B >: A](b: => B) = this match {
    case Okay(a) => a
    case Kayo(_) => b
    case Checked.None(_) => b
  }
  def map[B](f: A => B): Checked[B, R] = this match {
    case Okay(a) => Okay(f(a))
    case Kayo(r) => Kayo(r)
    case Checked.None(Okay(a)) => Checked.None(Okay(f(a)))
  }
  def flatMap[B, S >: R](f: A => Checked[B, S]): Checked[B, S] = this match {
    case Okay(a) => f(a)
    case Kayo(s) => Kayo(s)
    case Checked.None(Okay(a)) => f(a) match {
      case okay @ Okay(_) => Checked.None(okay)
      case kayo @ Kayo(_) => kayo
      case kayo @ Checked.None(_) => kayo
    }
  }
  def withFilter(p: A => Boolean): Checked[A, R] = this match {
    case okay @ Okay(a) => if (p(a)) okay else Checked.None(okay)
    case kayo @ Kayo(_) => kayo
    case kayo @ Checked.None(_) => kayo
  }
  def foreach[B](f: A => B): Unit = this match {
    case Okay(a) => f(a)
    case Kayo(_) =>
    case Checked.None(_) =>
  }
}

private object Checked {
  case class None[+A, +R](okay: Okay[A, R]) extends Checked[A, R] {
    def isOkay = false
  }
}

final case class Okay[+A, +R](a: A) extends Checked[A, R] {
  def isOkay = true
}

final case class Kayo[+A, +R](r: R) extends Checked[A, R] {
  def isOkay = false
}

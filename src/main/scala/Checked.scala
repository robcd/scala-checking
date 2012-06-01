package org.lafros.scala

sealed abstract class Checked[+A, +R] {
  def isOkay: Boolean
  def get: A = this match {
    case Okay(a) => a
    case Kayo(_) => throw new NoSuchElementException("Kayo.get")
    case Checked.None => throw new NoSuchElementException("None.get")
  }
  def reason: R = this match {
    case Okay(_) => throw new NoSuchElementException("Okay.reason")
    case Kayo(r) => r
    case Checked.None => throw new NoSuchElementException("None.reason")
  }
  def getOrElse[B >: A](b: => B) = this match {
    case Okay(a) => a
    case _ => b
  }
  def map[B](f: A => B): Checked[B, R] = this match {
    case Okay(a) => Okay(f(a))
    case Kayo(r) => Kayo(r)
    case Checked.None => Checked.None
  }
  def flatMap[B, S >: R](f: A => Checked[B, S]): Checked[B, S] = this match {
    case Okay(a) => f(a)
    case Kayo(s) => Kayo(s)
    case Checked.None => Checked.None
  }
  def withFilter(p: A => Boolean): Checked[A, R] = this match {
    case Okay(a) if !p(a) => Checked.None
    case _ => this
  }
  def foreach[B](f: A => B): Unit = this match {
    case Okay(a) => f(a)
    case _ =>
  }
}

private object Checked {
  case object None extends Checked[Nothing, Nothing] {
    def isOkay = false
  }
}

final case class Okay[+A, +R](a: A) extends Checked[A, R] {
  def isOkay = true
}

final case class Kayo[+A, +R](r: R) extends Checked[A, R] {
  def isOkay = false
}

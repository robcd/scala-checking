package org.lafros.scala

sealed abstract class Checked[+A, +R] {
  def isOkay: Boolean
  def get: A = this match {
    case Okay(a) => a
    case Kayo(_) => throw new NoSuchElementException("Kayo.get")
    case OkayAsKayo(_) => throw new NoSuchElementException("OkayAsKayo.get")
  }
  def reason: R = this match {
    case Okay(_) => throw new NoSuchElementException("Okay.reason")
    case Kayo(r) => r
    case OkayAsKayo(_) => throw new NoSuchElementException("OkayAsKayo.reason")
  }
  def getOrElse[B >: A](b: => B) = this match {
    case Okay(a) => a
    case Kayo(_) => b
    case OkayAsKayo(_) => b
  }
  def map[B](f: A => B): Checked[B, R] = this match {
    case Okay(a) => Okay(f(a))
    case Kayo(r) => Kayo(r)
    case OkayAsKayo(Okay(a)) => OkayAsKayo(Okay(f(a)))
  }
  def flatMap[B, S >: R](f: A => Checked[B, S]): Checked[B, S] = this match {
    case Okay(a) => f(a)
    case Kayo(s) => Kayo(s)
    case OkayAsKayo(Okay(a)) => f(a) match {
      case okay @ Okay(_) => OkayAsKayo(okay)
      case kayo @ Kayo(_) => kayo
      case kayo @ OkayAsKayo(_) => kayo
    }
  }
  def withFilter(p: A => Boolean): Checked[A, R] = this match {
    case okay @ Okay(a) => if (p(a)) okay else OkayAsKayo(okay)
    case kayo @ Kayo(_) => kayo
    case kayo @ OkayAsKayo(_) => kayo
  }
  def foreach[B](f: A => B): Unit = this match {
    case Okay(a) => f(a)
    case Kayo(_) =>
    case OkayAsKayo(_) =>
  }
}

final case class Okay[+A, +R](a: A) extends Checked[A, R] {
  def isOkay = true
}

final case class Kayo[+A, +R](r: R) extends Checked[A, R] {
  def isOkay = false
}

private final case class OkayAsKayo[+A, +R](okay: Okay[A, R]) extends Checked[A, R] {
  def isOkay = false
}

package org.lafros.scala

trait Checking {
  sealed abstract class Checked[+A, +R] {
    def isOkay: Boolean
    def get: A = this match {
      case Okay(a) => a
      case Reason(_) => throw new NoSuchElementException("Reason.get")
      case Checked.None => throw new NoSuchElementException("None.get")
    }
    def reason: R = this match {
      case Okay(_) => throw new NoSuchElementException("Okay.reason")
      case Reason(r) => r
      case Checked.None => throw new NoSuchElementException("None.reason")
    }
    def getOrElse[B >: A](b: => B) = this match {
      case Okay(a) => a
      case _ => b
    }
    def map[B](f: A => B): Checked[B, R] = this match {
      case Okay(a) => Okay(f(a))
      case Reason(r) => Reason(r)
      case Checked.None => Checked.None
    }
    def flatMap[B, S >: R](f: A => Checked[B, S]): Checked[B, S] = this match {
      case Okay(a) => f(a)
      case Reason(s) => Reason(s)
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

  final case class Reason[+A, +R](r: R) extends Checked[A, R] {
    def isOkay = false
  }

  abstract class CheckedLift[T] {
    def toOkay  [R]:   Okay[T, R]
    def toReason[A]: Reason[A, T]
    def failFast  [R](checks: (T => Checked[T, R])*): Checked[T, R]
    def failSlowly[R](checks: (T => Checked[T, R])*): Checked[T, Iterable[R]]
  }

  implicit def any2CheckedLift[T](any: T): CheckedLift[T] = new CheckedLift[T] {
    def toOkay  [R] = Okay[T, R](any)
    def toReason[A] = Reason[A, T](any)
    def failFast[R](checks: (T => Checked[T, R])*) = {
      var opt: Option[Reason[T, R]] = None
      checks.find { _.apply(any) match {
          case reason @ Reason(_) => opt = Some(reason); true
          case _ => false
        }
      }
      opt.getOrElse(Okay(any))
    }
    def failSlowly[R](checks: (T => Checked[T, R])*) = {
      val rs: Iterable[R] = for {
        check <- checks
        res = check(any)
        if !res.isOkay
      } yield res.reason
      if (rs.isEmpty) Okay(any) else Reason(rs)
    }
  }

  //type L
}

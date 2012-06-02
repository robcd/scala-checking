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
    def ff [R](checks: (T => Checked[T, R])*): Checked[T, R]
    def fs [R](checks: (T => Checked[T, R])*): Checked[T, Iterable[R]]
  }

  implicit def any2CheckedLift[T](any: T): CheckedLift[T] = new CheckedLift[T] {
    def toOkay  [R] = Okay[T, R](any)
    def toReason[A] = Reason[A, T](any)
    def ff[R](checks: (T => Checked[T, R])*) = {
      var opt: Option[Reason[T, R]] = None
      checks.find { _.apply(any) match {
          case reason @ Reason(_) => opt = Some(reason); true
          case _ => false
        }
      }
      opt.getOrElse(Okay(any))
    }
    def fs[R](checks: (T => Checked[T, R])*) = {
      val rs: Iterable[R] = for {
        check <- checks
        res = check(any)
        if !res.isOkay
      } yield res.reason
      if (rs.isEmpty) Okay(any) else Reason(rs)
    }
  }

  type R

  trait FailFastAppFunct[A, B] {
    /**
     * handles a fail-slowly result, where Result contains an Iterable. */
    def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]): Checked[B, R]
    /**
     * confers fail-fast applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts, failing as soon as the first Reason is encountered. */
    def <*>(checked: Checked[A, R]): Checked[B, R]
  }
  trait FailSlowlyAppFunct[A, B] {
    /**
     * handles a fail-slow result, where Left contains a List. */
    def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]): Checked[B, Iterable[R]]
    /**
     * confers fail-slowly applicative-functor status: lets you apply a FunctionN to N values
     * in N contexts, continuing to accumulate further Reasons after the first is encountered. */
    def <*>(checked: Checked[A, R]): Checked[B, Iterable[R]]
  }
  /**
   * lifts f, which must be curried, into an Okay, for fail-fast application. */
  def ff[A, B](f: A => B) = Okay[A => B, R](f)
  /**
   * as above, but for fail-slowly application. */
  def fs[A, B](f: A => B) = Okay[A => B, Iterable[R]](f)

  implicit def checkedOfFun2ffap[A, B](f: Checked[A => B, R]): FailFastAppFunct[A, B] =
    new FailFastAppFunct[A, B] {
      def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]) = checked match {
        case Reason(rs)   => <*>(Reason[A, R](rs.head))
        case   Okay(a)    => <*>(Okay[A, R](a))
        case Checked.None => f match {
          case Reason(r) => Reason(r)
          case _ => Checked.None
        }
      }
      def <*>(checked: Checked[A, R]) = ((f, checked): @unchecked) match {
        case (Reason(r),          _) => Reason(r)
        case (Okay(_),    Reason(r)) => Reason(r)
        case (Okay(f),      Okay(a)) => Okay(f(a))
        case (Checked.None,       _) => Checked.None
      }
    }
  implicit def checkedOfFun2fsap[A, B](f: Checked[A => B, Iterable[R]]): FailSlowlyAppFunct[A, B] =
    new FailSlowlyAppFunct[A, B] {
      def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]) =
        (f, checked) match {
          case (Reason(rs1), Reason(rs2)) => Reason(rs1 ++ rs2)
          case (Reason(rs),      Okay(_)) => Reason(rs)
          case (Reason(rs), Checked.None) => Reason(rs)
          case (Okay(_),      Reason(rs)) => Reason(rs)
          case (Okay(f),         Okay(a)) => Okay(f(a))
          case (Checked.None, Reason(rs)) => Reason(rs)
          case (Checked.None,    Okay(_)) => Checked.None
          case (_,          Checked.None) => Checked.None
        }
      def <*>(checked: Checked[A, R]) = ((f, checked): @unchecked) match {
        case (Reason(rs),   Reason(r)) => Reason(rs ++ Iterable(r))
        case (Reason(rs),     Okay(_)) => Reason(rs)
        case (Okay(_),      Reason(r)) => Reason(Iterable(r))
        case (Okay(f),        Okay(a)) => Okay(f(a))
        case (Checked.None, Reason(r)) => Reason(Iterable(r))
        case (Checked.None,   Okay(_)) => Checked.None
      }
    }
}

object Checking extends Checking {
  type L = String
}

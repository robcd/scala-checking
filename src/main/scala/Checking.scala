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
package org.lafros.scala
/**
 * Mix-in alternative to importing Checking._, when wishing to use the nested class, Checked.
 */
trait Checking {
  /**
   * Yet another 'biased Either' candidate.
   *
   * For use as the return type, in place of A, wherever an exception might otherwise be thrown
   * and a more 'functional' style is preferred.
   *
   * The value of A to be returned is returned wrapped in an instance of one of only two
   * concrete subclasses of Checked, depending on whether or not an exceptional condition arose.
   *
   * If there was no exception, this will be the Okay subclass, otherwise, the Reason one.
   */
  sealed abstract class Checked[+A, +R] {
    def isOkay: Boolean
    /**
     * throws a NoSuchElementException if this is a Reason.
     */
    def get: A = this match {
      case Okay(a) => a
      case Reason(_) => throw new NoSuchElementException("Reason.get")
    }
    /**
     * throws a NoSuchElementException if this is an Okay.
     */
    def reason: R = this match {
      case Okay(_) => throw new NoSuchElementException("Okay.reason")
      case Reason(r) => r
    }
    def getOrElse[B >: A](b: => B) = this match {
      case Okay(a) => a
      case Reason(_) => b
    }
    def map[B](f: A => B): Checked[B, R] = this match {
      case Okay(a) => Okay(f(a))
      case Reason(r) => Reason(r)
    }
    def flatMap[B, S >: R](f: A => Checked[B, S]): Checked[B, S] = this match {
      case Okay(a) => f(a)
      case Reason(s) => Reason(s)
    }
    def foreach[B](f: A => B): Unit = this match {
      case Okay(a) => f(a)
      case Reason(_) =>
    }
    def toOption: Option[A] = if (isOkay) Some(get) else None
  }
  /**
   * see Checked.
   */
  final case class Okay[+A, +R](a: A) extends Checked[A, R] {
    def isOkay = true
  }
  /**
   * see Checked.
   */
  final case class Reason[+A, +R](r: R) extends Checked[A, R] {
    def isOkay = false
  }
  /**
   * type to which some T is converted by the implicit conversion, any2CheckedLift.
   */
  abstract class CheckedLift[T] {
    def toOkay  [R]:   Okay[T, R]
    def toReason[A]: Reason[A, T]
    /**
     * ff as in fail-fast: applies checks to the target T, in a fail-fast manner (stopping after
     * the first Reason is encountered).
     */
    def ff [R](checks: (T => Checked[T, R])*): Checked[T, R]
    /**
     * fs as in fail-slowly: applies checks to the target T, in a fail-slowly manner (continuing
     * after the first Reason is encounted).
     */
    def fs [R](checks: (T => Checked[T, R])*): Checked[T, Iterable[R]]
  }
  /**
   * See CheckedLift.
   */
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
  /**
   * the type of value that will be wrapped in a Reason, that only needs to be supplied when
   * using the implicitly-supplied operator, <*>.
   */
  type R

  trait FailFastAppFunct[A, B] {
    /**
     * handles a fail-slowly result where Reason contains an Iterable. */
    def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]): Checked[B, R]
    /**
     * confers fail-fast applicative-functor status: lets you apply a FunctionN to N values in N
     * contexts, failing as soon as the first Reason is encountered. */
    def <*>(checked: Checked[A, R]): Checked[B, R]
  }
  trait FailSlowlyAppFunct[A, B] {
    /**
     * handles a fail-slowly result where Reason contains an Iterable. */
    def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]): Checked[B, Iterable[R]]
    /**
     * confers fail-slowly applicative-functor status: lets you apply a FunctionN to N values
     * in N contexts, continuing to accumulate further Reasons after the first is encountered. */
    def <*>(checked: Checked[A, R]): Checked[B, Iterable[R]]
  }
  /**
   * lifts f, which must be curried, into an Okay, for fail-fast application (stopping after
   * the first Reason is encountered). */
  def ff[A, B](f: A => B) = Okay[A => B, R](f)
  /**
   * as above, but for fail-slowly application (continuing after the first Reason is
   * encountered). */
  def fs[A, B](f: A => B) = Okay[A => B, Iterable[R]](f)

  implicit def checkedOfFun2ffap[A, B](f: Checked[A => B, R]): FailFastAppFunct[A, B] =
    new FailFastAppFunct[A, B] {
      def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]) = checked match {
        case   Okay(a)    => <*>(Okay[A, R](a))
        case Reason(rs)   => <*>(Reason[A, R](rs.head))
      }
      def <*>(checked: Checked[A, R]) = (f, checked) match {
        case (  Okay(f),      Okay(a)) => Okay(f(a))
        case (  Okay(_),    Reason(r)) => Reason(r)
        case (Reason(r),            _) => Reason(r)
      }
    }
  implicit def checkedOfFun2fsap[A, B](f: Checked[A => B, Iterable[R]]): FailSlowlyAppFunct[A, B] =
    new FailSlowlyAppFunct[A, B] {
      def <*>[Rs](checked: Checked[A, Rs])(implicit ev: Rs <:< Iterable[R]) =
        (f, checked) match {
          case (Okay(f),         Okay(a)) => Okay(f(a))
          case (Okay(_),      Reason(rs)) => Reason(rs)
          case (Reason(rs),      Okay(_)) => Reason(rs)
          case (Reason(rs1), Reason(rs2)) => Reason(rs1 ++ rs2)
        }
      def <*>(checked: Checked[A, R]) = (f, checked) match {
        case (Okay(f),         Okay(a)) => Okay(f(a))
        case (Okay(_),       Reason(r)) => Reason(Iterable(r))
        case (Reason(rs),      Okay(_)) => Reason(rs)
        case (Reason(rs),    Reason(r)) => Reason(rs ++ Iterable(r))
      }
    }
}
/**
 * companion object provided as a convenience, that may be imported, instead of mixing in the
 * trait, and having R set to String.
 */
object Checking extends Checking {
  type R = String
}

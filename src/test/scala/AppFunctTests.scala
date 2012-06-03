import org.lafros.scala.Checking
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class AppFunctTests extends FunSuite with ShouldMatchers {
  abstract class Spec {
    this: Checking =>
    type A
    type C = Checked[A, R]
    def f(a1: A)(a2: A): A
    def a1: C
    def a2: C

    def res1 = ff(f) <*> a1 <*> a2
    def res2 = fs(f) <*> a1 <*> a2
    // NB defs instead of vals since depend on abstract members (which might get impl'ted in
    // terms of vals, which would otherwise get initialised too late)
  }

  abstract class Case extends Spec with Checking {
    type A = Int
    type R = String
    def f(a1: A)(a2: A) = a1*a2
  }

  val a1Msg = "Couldn't obtain a1!"
  val a2Msg = "Couldn't obtain a2!"

  test("a1, a2 both Okay") {
    new Case {
      def a1 = Okay(2)
      def a2 = Okay(3)

      res1 should equal(Okay(6))
      res2 should equal(Okay(6))
    }
  }
  test("a1 Okay, a2 unobtainable") {
    new Case {
      def a1 = Okay(2)
      def a2 = Reason(a2Msg)

      res1 should equal(Reason(a2Msg))
      res2 should equal(Reason(Iterable(a2Msg)))
    }
  }
  test("a1 unobtainable, a2 Okay") {
    new Case {
      def a1 = Reason(a1Msg)
      def a2 = Okay(3)

      res1 should equal(Reason(a1Msg))
      res2 should equal(Reason(Iterable(a1Msg)))
    }
  }
  test("a1, a2 both unobtainable") {
    new Case {
      def a1 = Reason(a1Msg)
      def a2 = Reason(a2Msg)

      res1 should equal(Reason(a1Msg))
      res2 should equal(Reason(Iterable(a1Msg, a2Msg)))
    }
  }
  test("a1 Okay, a2 filtered-out") {
    new Case {
      def a1 = Okay(2)
      def a2 = Okay(3) withFilter (_ < 0)

      res1.toString should equal("None")
      res2.toString should equal("None")
    }
  }
  test("a1 unobtainable, a2 filtered-out") {
    new Case {
      def a1 = Reason(a1Msg)
      def a2 = Okay(3) withFilter (_ < 0)

      res1 should equal(Reason(a1Msg))
      res2 should equal(Reason(Iterable(a1Msg)))
    }
  }
  test("a1 filtered-out, a2 Okay") {
    new Case {
      def a1 = Okay(2) withFilter (_ < 0)
      def a2 = Okay(3)

      res1.toString should equal("None")
      res2.toString should equal("None")
    }
  }
  test("a1 filtered-out, a2 unobtainable") {
    new Case {
      def a1 = Okay(2) withFilter (_ < 0)
      def a2 = Reason(a2Msg)

      res1.toString should equal("None")
      res2          should equal(Reason(Iterable(a2Msg)))
    }
  }
  test("a1, a2 both filtered out") {
    new Case {
      def a1 = Okay(2) withFilter (_ < 0)
      def a2 = Okay(3) withFilter (_ < 0)

      res1.toString should equal("None")
      res2.toString should equal("None")
    }
  }
}

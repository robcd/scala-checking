import org.lafros.scala.Checking
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class AppFunctTests extends FunSuite with ShouldMatchers {
  abstract class Spec {
    this: Checking =>
    type A
    type C[A] = Checked[A, R]
    def f(a1: A)(a2: A): A
    def a1: C[A]
    def a2: C[A]

    val res1 = ff(f) <*> a1 <*> a2
    val res2 = fs(f) <*> a1 <*> a2
  }

  abstract class Case extends Spec with Checking {
    type A = Int
    type R = String
    def f(a1: A)(a2: A) = a1*a2
  }

  val no_a1 = "Couldn't obtain a1!"
  val no_a2 = "Couldn't obtain a2!"

  test("a1, a2 both unobtainable") {
    new Case {
      def a1 = Reason(no_a1)
      def a2 = Reason(no_a2)

      res1 should equal(Reason(no_a1))
      res2 should equal(Reason(Iterable(no_a1, no_a2)))
    }
  }
  test("a1 Okay, a2 unobtainable") {
    new Case {
      def a1 = Okay(2)
      def a2 = Reason(no_a2)

      res1 should equal(Reason(no_a2))
      res2 should equal(Reason(Iterable(no_a2)))
    }
  }
  test("a1, a2 both Okay") {
    new Case {
      def a1 = Okay(2)
      def a2 = Okay(3)

      res1 should equal(Okay(6))
      res2 should equal(Okay(6))
    }
  }
}

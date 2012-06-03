import org.lafros.scala.Checking
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class AppFunctTestsWithChecks extends FunSuite with ShouldMatchers {
  abstract class Spec {
    this: Checking =>
    type A
    type C = Checked[A, R]
    def f(a1: A)(a2: A): A
    def a1: A
    def a2: A
    def check1(a: A): C
    def check2(a: A): C

    def res1 = ff(f) <*> a1.ff(check1, check2) <*> a2.ff(check1, check2)

    def res2 = fs(f) <*> a1.ff(check1, check2) <*> a2.ff(check1, check2)

    def res3 = fs(f) <*> a1.fs(check1, check2) <*> a2.fs(check1, check2)

    def res4 = fs(f) <*> a1.ff(check1, check2) <*> a2.fs(check1, check2)
    // NB defs instead of vals since depend on abstract members (which might get impl'ted in
    // terms of vals, which would otherwise get initialised too late)
  }

  abstract class Case extends Spec with Checking {
    val mustBe = "must be at least one item"
    val nameNull = "name was null"
    val nameEmpty = "empty name"

    type A = (Int, String)
    type R = String
    def f(a1: A)(a2: A) = (a1._1 + a2._1, a2._2 +" & "+ a2._2 +" items")
    def check1(a: A) = if (a._1 > 0) Okay(a) else Reason(mustBe)
    def check2(a: A) = a._2 match {
      case null => Reason(nameNull)
      case "" => Reason(nameEmpty)
      case _ => Okay(a)
    }
  }

  test("(0, empty) (-1, null)") {
    new Case {
      def a1 = (0, "")
      def a2 = (-1, null)

      res1 should equal(Reason(mustBe))
      res2 should equal(Reason(Iterable(mustBe, mustBe)))
      res3 should equal(Reason(Iterable(mustBe, nameEmpty, mustBe, nameNull)))
      res4 should equal(Reason(Iterable(mustBe, mustBe, nameNull)))
    }
  }
}

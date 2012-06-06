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
    def f(a1: A)(a2: A) = (a1._1 + a2._1, a1._2 +" & "+ a2._2 +" items")
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

  test("(2, null) (0, tenor)") {
    new Case {
      def a1 = (2, null)
      def a2 = (0, "tenor")

      res1 should equal(Reason(nameNull))
      res2 should equal(Reason(Iterable(nameNull, mustBe)))
      res3 should equal(Reason(Iterable(nameNull, mustBe)))
      res4 should equal(Reason(Iterable(nameNull, mustBe)))
    }
  }

  test("(2, alto) (3, empty)") {
    new Case {
      def a1 = (2, "alto")
      def a2 = (3, "")

      res1 should equal(Reason(nameEmpty))
      res2 should equal(Reason(Iterable(nameEmpty)))
      res3 should equal(Reason(Iterable(nameEmpty)))
      res4 should equal(Reason(Iterable(nameEmpty)))
    }
  }
  test("(2, alto) (3, tenor)") {
    new Case {
      def a1 = (2, "alto")
      def a2 = (3, "tenor")

      res1 should equal(Okay((5, "alto & tenor items")))
      res2 should equal(Okay((5, "alto & tenor items")))
      res3 should equal(Okay((5, "alto & tenor items")))
      res4 should equal(Okay((5, "alto & tenor items")))
    }
  }
}

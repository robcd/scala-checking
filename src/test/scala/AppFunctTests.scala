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
}

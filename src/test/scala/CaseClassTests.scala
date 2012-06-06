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

class CaseClassTests extends FunSuite with ShouldMatchers with Checking {
  case class MyCaseClass(qty: Int, name: String)

  object MyCaseClass {
    type Ch[A] = Checked[A, String]

    def checkQty(qty: Int): Ch[Int] =
      if (qty > 0) Okay(qty) else Reason("qty must be > 0")

    def checkName(name: String): Ch[String] = name match {
      case null => Reason("name was null")
      case ""   => Reason("empty name")
      case _    => Okay(name)
    }
  }

  type R = String
  // required by Checking in order to use Checked[A, R]'s <*>

  abstract class Case {
    def qty: Int
    def name: String

    import MyCaseClass._
    val res = fs((apply _).curried) <*> checkQty(qty) <*> checkName(name)
  }

  test("0, null") {
    new Case {
      def qty = 0
      def name = null

      res should equal(Reason(Iterable("qty must be > 0", "name was null")))
    }
  }
  test("2, empty") {
    new Case {
      def qty = 2
      def name = ""

      res should equal(Reason(Iterable("empty name")))
    }
  }
  test("-1, mouthpiece") {
    new Case {
      def qty = -1
      def name = "mouthpiece"

      res should equal(Reason(Iterable("qty must be > 0")))
    }
  }
  test("2, reed") {
    new Case {
      def qty = 2
      def name = "reed"

      res should equal(Okay(MyCaseClass(2, "reed")))
    }
  }
}

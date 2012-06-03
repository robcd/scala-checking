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

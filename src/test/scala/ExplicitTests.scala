import org.lafros.scala.Checking
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class ExplicitTests extends FunSuite with ShouldMatchers with Checking {
  type Ch = Checked[Int, String]

  def gt0(n: Int): Ch = if (n > 0) Okay(n) else Reason("n must be > 0: "+ n)

  test("withFilter") {
    val a = 1
    var res = 0

    gt0(a) withFilter(_ < 0) map { res = _ }

    res should equal(0)

    gt0(a) withFilter(_ > 0) map { res = _ }

    res should equal(1)
  }
}

import org.lafros.scala.Checking
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class LiftTests extends FunSuite with ShouldMatchers with Checking {
  test("toOkay") {
    val n = 1
    n.toOkay         should equal(Okay[Int,    Any](n))
    n.toOkay[String] should equal(Okay[Int, String](n))
  }
  test("toReason") {
    val msg = "no good"
    msg.toReason       should equal(Reason[Any,  String](msg))
    msg.toReason[Int]  should equal(Reason[Int,  String](msg))
  }

  type A = (Boolean, Int)
  type C = Checked[A, String]
  def checkTrue(a: A): C = if (a._1 == true) Okay(a) else Reason("Boolean field was "+ a._1)
  def checkPositive(a: A): C = if (a._2 > 0) Okay(a) else Reason("Int field not > 0: "+ a._2)
  val badValue: A = (false, -1)
  val goodValue: A = (true, 1)
  val checks = Seq(checkTrue _, checkPositive _)
  object expected {
    object fast {
      val okay = Okay[A, String](goodValue)
      val reason = Reason[A, String]("Boolean field was "+ badValue._1)
    }
    object slow {
      val okay = Okay[A, List[String]](goodValue)
      val reason = {
        val msgs = List("Boolean field was "+ badValue._1, "Int field not > 0: "+ badValue._2)
        Reason[A, List[String]](msgs)
      }
    }
  }

  test("failFast (without mapping) bad value") {
    badValue.failFast(checkTrue, checkPositive) should equal(expected.fast.reason)
    badValue.failFast(checks: _*)               should equal(expected.fast.reason)
  }
  test("failSlowly (without mapping) bad value") {
    badValue.failSlowly(checkTrue, checkPositive) should equal(expected.slow.reason)
    badValue.failSlowly(checks: _*)               should equal(expected.slow.reason)
  }
  test("failFast (without mapping) good value") {
    goodValue.failFast(checkTrue, checkPositive) should equal(expected.fast.okay)
    goodValue.failFast(checks: _*)               should equal(expected.fast.okay)
  }
  test("failSlowly (without mapping) good value") {
    goodValue.failSlowly(checkTrue, checkPositive) should equal(expected.slow.okay)
    goodValue.failSlowly(checks: _*)               should equal(expected.slow.okay)
  }
  test("failFast + map, bad value") {
    def f(a: A) = a.toString
    badValue.failFast(checkTrue, checkPositive).map(f) should equal(expected.fast.reason)
    badValue.failFast(checks: _*).map(f)               should equal(expected.fast.reason)
  }
  test("failSlowly + map, bad value") {
    def f(a: A) = a.toString
    badValue.failSlowly(checkTrue, checkPositive).map(f) should equal(expected.slow.reason)
    badValue.failSlowly(checks: _*).map(f)               should equal(expected.slow.reason)
  }
  test("failFast + map, good value") {
    val expectedValue = Okay[String, String](goodValue.toString)
    def f(a: A) = a.toString
    goodValue.failFast(checkTrue, checkPositive).map(f) should equal(expectedValue)
    goodValue.failFast(checks: _*).map(f)               should equal(expectedValue)
  }
  test("failSlowly + map, good value") {
    val expectedValue = Okay[String, List[String]](goodValue.toString)
    def f(a: A) = a.toString
    goodValue.failSlowly(checkTrue, checkPositive).map(f) should equal(expectedValue)
    goodValue.failSlowly(checks: _*).map(f)               should equal(expectedValue)
  }
}

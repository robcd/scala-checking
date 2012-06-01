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
  // test("slowCheck (without mapping) bad value") {
  //   badValue.slowCheck(checkTrue, checkPositive) should equal(expected.slow.fail)
  //   badValue.slowCheck(checks: _*)               should equal(expected.slow.fail)
  // }
  // test("fastCheck (without mapping) good value") {
  //   goodValue.fastCheck(checkTrue, checkPositive) should equal(expected.fast.succ)
  //   goodValue.fastCheck(checks: _*)               should equal(expected.fast.succ)
  // }
  // test("slowCheck (without mapping) good value") {
  //   goodValue.slowCheck(checkTrue, checkPositive) should equal(expected.slow.succ)
  //   goodValue.slowCheck(checks: _*)               should equal(expected.slow.succ)
  // }
  // test("fastCheckAndMap bad value") {
  //   def f(t: T) = t.toString
  //   badValue.fastCheckAndMap(checkTrue, checkPositive)(f) should equal(expected.fast.fail)
  //   badValue.fastCheckAndMap(checks: _*)(f)               should equal(expected.fast.fail)
  // }
  // test("slowCheckAndMap bad value") {
  //   def f(t: T) = t.toString
  //   badValue.slowCheckAndMap(checkTrue, checkPositive)(f) should equal(expected.slow.fail)
  //   badValue.slowCheckAndMap(checks: _*)(f)               should equal(expected.slow.fail)
  // }
  // test("fastCheckAndMap good value") {
  //   val expectedValue = Right[List[String], String](goodValue.toString)
  //   def f(t: T) = t.toString
  //   goodValue.fastCheckAndMap(checkTrue, checkPositive)(f) should equal(expectedValue)
  //   goodValue.fastCheckAndMap(checks: _*)(f)               should equal(expectedValue)
  // }
  // test("slowCheckAndMap good value") {
  //   val expectedValue = Right[List[String], String](goodValue.toString)
  //   def f(t: T) = t.toString
  //   goodValue.slowCheckAndMap(checkTrue, checkPositive)(f) should equal(expectedValue)
  //   goodValue.slowCheckAndMap(checks: _*)(f)               should equal(expectedValue)
  // }
}

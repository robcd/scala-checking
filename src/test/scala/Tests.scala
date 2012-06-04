import org.lafros.scala.Checking
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class Tests extends FunSuite with ShouldMatchers with Checking {
  // n.b. Checking's abstract type, R need not be supplied for a concrete class to compiled and
  // even instantiated, provided we don't refer to it - this we only do when using <*>

  test("foreach - okay") {
    val checked: Checked[Int, String] = Okay(1)
    var res = 0
    for {
      a <- checked
      b = a + 1
    } res = b

    res should equal(2)
  }

  test("foreach - reason") {
    val checked: Checked[Int, String] = Reason("er")
    var res = 0
    for {
      a <- checked
      b = a + 1
    } res = b

    res should equal(0)
  }

  test("map - okay") {
    val checked: Checked[Int, String] = Okay(1)
    val res = for {
      a <- checked
      b = a + 1
    } yield b

    res should equal(Okay(2))
    res.get should equal(2)
    res.getOrElse(0) should equal(2)
    val thrown = intercept[NoSuchElementException] {
      res.reason
    }
    thrown.getMessage should equal("Okay.reason")
    // left.forall(_ == 1) should be(false)
    // left.forall(_ == 2) should be(true)
    // left.exists(_ == 1) should be(false)
    // left.exists(_ == 2) should be(true)
    // left.toSeq should equal(Seq(2))
    // left.toOption should equal(Some(2))
  }

  test("map - reason") {
    val checked: Checked[Int, String] = Reason("er")
    val res = for {
      a <- checked
      b = a + 1
    } yield b

    res should equal(Reason("er"))
    res.reason should equal("er")
    res.getOrElse(0) should equal(0)
    val thrown = intercept[NoSuchElementException] {
      res.get
    }
    thrown.getMessage should equal("Reason.get")
    // left.forall(_ == 1) should be(false)
    // left.forall(_ == 2) should be(true)
    // left.exists(_ == 1) should be(false)
    // left.exists(_ == 2) should be(true)
    // left.toSeq should equal(Seq(2))
    // left.toOption should equal(Some(2))
  }

  def gt0(n: Int): Checked[Int, String] = if (n > 0) Okay(n) else Reason("n must be > 0: "+ n)
  def gt1(n: Int): Checked[Int, String] = if (n > 1) Okay(n) else Reason("n must be > 1: "+ n)

  test("foreach, two generators - okay 1") {
    var res = 0
    val a = 2
    for {
      b <- gt0(a)
      c <- gt1(b)
    } res = c

    res should equal(2)
  }

  test("foreach, two generators - okay 2") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
    } res = d

    res should equal(2)
  }

  test("foreach two generators - okay 3") {
    var res = 0
    val a = 2
    for {
      b <- gt0(a)
      c <- gt1(b)
      d = c + 1
    } res = d

    res should equal(3)
  }

  test("foreach, two generators - reason 1") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c <- gt1(b)
    } res = c

    res should equal(0)
  }

  test("foreach, two generators - reason 2") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c = b - 1
      d <- gt1(c)
    } res = d

    res should equal(0)
  }

  test("foreach, two generators - reason 3") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c <- gt1(b)
      d = c + 1
    } res = d

    res should equal(0)
  }

  test("map, two generators - okay 1") {
    val a = 2
    val res = for {
      b <- gt0(a)
      c <- gt1(b)
    } yield c

    res should equal(Okay(a))
  }

  test("map, two generators - okay 2") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
    } yield d

    res should equal(Okay(2))
  }

  test("map, two generators - okay 3") {
    val a = 2
    val res = for {
      b <- gt0(a)
      c <- gt1(b)
      d = c + 1
    } yield d

    res should equal(Okay(3))
  }

  test("map, two generators - reason 1") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c <- gt1(b)
    } yield c

    res should equal(Reason("n must be > 1: 1"))
  }

  test("map, two generators - reason 2") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b - 1
      d <- gt1(c)
    } yield d

    res should equal(Reason("n must be > 1: 0"))
  }

  test("map, two generators - reason 3") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c <- gt1(b)
      d = c + 1
    } yield d

    res should equal(Reason("n must be > 1: 1"))
  }
}


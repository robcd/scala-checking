import org.lafros.scala.{Checked, Okay, Kayo}
import org.scalatest.{FunSuite, matchers}
import matchers.ShouldMatchers

class Tests extends FunSuite with ShouldMatchers {
  test("foreach true") {
    val checked: Checked[Int, String] = Okay(1)
    var res = 0
    for {
      a <- checked
      b = a + 1
      if b > 0
    } res = b

    res should equal(2)
  }

  test("foreach false") {
    val checked: Checked[Int, String] = Okay(1)
    var res = 0
    for {
      a <- checked
      b = a + 1
      if b < 0
    } res = b

    res should equal(0)
  }

  test("map true") {
    val checked: Checked[Int, String] = Okay(1)
    val res = for {
      a <- checked
      b = a + 1
      if b > 0
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

  test("map false") {
    val checked: Checked[Int, String] = Okay(1)
    val res = for {
      a <- checked
      b = a + 1
      if b < 0
    } yield b
    res.toString should equal("None")
    ;{
      val thrown = intercept[NoSuchElementException] {
        res.get
      }
      thrown.getMessage should equal("None.get")
    }
    res.getOrElse(0) should equal(0)
    ;{
      val thrown = intercept[NoSuchElementException] {
        res.reason
      }
      thrown.getMessage should equal("None.reason")
    }
    // left.forall(_ == 1) should be(true) // since no elements
    // left.forall(_ == 2) should be(true) // "
    // left.exists(_ == 1) should be(false) // "
    // left.exists(_ == 2) should be(false) // "
    // left.toSeq should equal(Seq())
    // left.toOption should equal(None)
  }



  def gt0(n: Int): Checked[Int, String] = if (n > 0) Okay(n) else Kayo("n must be > 0: "+ n)
  def gt1(n: Int): Checked[Int, String] = if (n > 1) Okay(n) else Kayo("n must be > 1: "+ n)

  test("two generators with foreach - okay 1") {
    var res = 0
    val a = 2
    for {
      b <- gt0(a)
      c <- gt1(b)
    } res = c
    res should equal(2)
  }

  test("two generators with foreach - okay 2") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
    } res = d
    res should equal(2)
  }

  test("two generators with foreach - kayo") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c <- gt1(b)
    } res = c
    res should equal(0)
  }

  test("two generators with foreach true") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
      if d > 0
    } res = d
    res should equal(2)
  }

  test("two generators with foreach false") {
    var res = 0
    val a = 1
    for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
      if d < 0
    } res = d
    res should equal(0)
  }

  test("two generators with map - okay 1") {
    val a = 2
    val res = for {
      b <- gt0(a)
      c <- gt1(b)
    } yield c
    res should equal(Okay(a))
  }

  test("two generators with map - okay 2") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
    } yield d
    res should equal(Okay(2))
  }

  test("two generators with map - fails") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c <- gt1(b)
    } yield c
    res should equal(Kayo("n must be > 1: 1"))
    res.getOrElse(0) should equal(0)
    res.reason should equal("n must be > 1: 1")
  }

  test("two generators with map true") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
      if d > 0
    } yield d
    res should equal(Okay(2))
  }

  test("two generators with map true 2") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b + 1
      if c > 0
      d <- gt1(c)
    } yield d
    res should equal(Okay(2))
  }

  test("two generators with map false") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b + 1
      d <- gt1(c)
      if d < 0
    } yield d
    res.toString should equal("None")
  }

  test("two generators with map false 2") {
    val a = 1
    val res = for {
      b <- gt0(a)
      c = b + 1
      if c < 0
      d <- gt1(c)
    } yield d
    res.toString should equal("None")
  }
}


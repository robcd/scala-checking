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

class TestsInvolvingOption extends FunSuite with ShouldMatchers with Checking {
  // n.b. Checking's abstract type, R need not be supplied for a concrete class to compiled and
  // even instantiated, provided we don't refer to it - this we only do when using <*>

  type Ch = Checked[Seq[String], Exception]

  test("foreach, Seq non-empty, head non-empty") {
    def read: Ch = Okay(Seq("1st", "2nd"))
    var res = ""
    for {
      ss <- read
      h <- ss.headOption // okay that this is now an Option, provided we don't use yield;
      if h != "" // can still use 'if' following an Option (which has a filter/withFilter)
    } res = h

    res should equal("1st")
  }

  // test("foreach, Seq non-empty, head non-empty 2") {
  //   def read: Ch = Okay(Seq("1st", "2nd"))
  //   var res = ""
  //   for {
  //     ss <- read
  //     //    ^
  //     // value withFilter is not a member of TestsInvolvingOption.this.Ch
  //     if !ss.empty
  //     h = ss.head
  //   } res = h

  //   res should equal("1st")
  // }

  test("foreach, Seq non-empty, head empty") {
    def read: Ch = Okay(Seq("", "2nd"))
    var res = ""
    for {
      ss <- read
      h <- ss.headOption
      if h != ""
    } res = h

    res should equal("")
  }

  test("foreach, Seq empty") {
    def read: Ch = Okay(Seq())
    var res = ""
    for {
      ss <- read
      h <- ss.headOption
      if h != ""
    } res = h

    res should equal("")
  }

  // test("map, Seq non-empty, head non-empty") {
  //   def read: Ch = Okay(Seq("1st", "2nd"))
  //   val res = for {
  //     ss <- read
  //     h <- ss.headOption
  //   //  ^
  //   // type mismatch;
  //   //  found   : Option[String]
  //   //  required: TestsInvolvingOption.this.Checked[?,?]
  //     if h != ""
  //   } yield h

  //   res should equal("1st")
  // }



  test("map, Seq non-empty, head non-empty") {
    def read: Ch = Okay(Seq("1st", "2nd"))
    val res = for {
      ss <- read.toOption
      h <- ss.headOption
      if h != ""
    } yield h

    res should equal(Some("1st"))
  }

  test("map, Seq non-empty, head empty") {
    def read: Ch = Okay(Seq("", "2nd"))
    val res = for {
      ss <- read.toOption
      h <- ss.headOption
      if h != ""
    } yield h

    res should equal(None)
  }

  test("map, Seq empty") {
    def read: Ch = Okay(Seq())
    val res = for {
      ss <- read.toOption
      h <- ss.headOption
      if h != ""
    } yield h

    res should equal(None)
  }

  test("foreach, Reason") {
    def read: Ch = Reason(new Exception("er"))
    var res = ""
    for {
      ss <- read
      h <- ss.headOption
      if h != ""
    } res = h

    res should equal("")
  }

  test("map, Reason") {
    def read: Ch = Reason(new Exception("er"))
    val res = for {
      ss <- read.toOption
      h <- ss.headOption
      if h != ""
    } yield h

    res should equal(None)
  }
}


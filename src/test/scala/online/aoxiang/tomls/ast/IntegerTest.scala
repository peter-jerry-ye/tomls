package online.aoxiang.tomls.ast

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class IntegerTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid integers") {
    val testCases = Table[String, Long](
      ("s", "n"),
      // Some corner cases
      (s"${Long.MaxValue}", Long.MaxValue),
      (s"${Long.MinValue}", Long.MinValue),

      // Cases in official document
      ("+99", 99),
      ("42", 42),
      ("0", 0),
      ("-17", -17),
      ("1_000", 1_000),
      ("5_349_221", 5_349_221),
      ("53_49_221", 53_49_221),
      ("1_2_3_4_5", 1_2_3_4_5),
      ("0xDEADBEEF", 0xdeadbeefL),
      ("0xdeadbeef", 0xdeadbeefL),
      ("0xdead_beef", 0xdead_beefL),
      ("0o01234567", java.lang.Long.parseLong("1234567", 8)),
      ("0o755", java.lang.Long.parseLong("755", 8)),
      ("0b11010110", java.lang.Long.parseLong("11010110", 2))
    )

    forAll(testCases) { (s: String, n: Long) =>
      TInteger.parser.parseAll(s).flatMap(_.value) should be(Right(n))
    }

    // hex, oct, bin without underscore
    forAll { (n: Long) =>
      TInteger.parser.parseAll(s"${n}").flatMap(_.value) should be(Right(n))
      whenever(n >= 0) {
        TInteger.parser.parseAll(s"${n}").flatMap(_.value) should be(Right(n))
        TInteger.parser.parseAll(s"0x${n.toHexString}").flatMap(_.value) should be(Right(n))
        TInteger.parser.parseAll(s"0o${n.toOctalString}").flatMap(_.value) should be(Right(n))
        TInteger.parser.parseAll(s"0b${n.toBinaryString}").flatMap(_.value) should be(Right(n))
      }
    }
  }

  test("Parse invalid integers") {
    val testCases = Table(
      "wrong inputs",
      "asdf",
      "0X123",
      "0B456",
      "0O11",
      "0o",
      "0x",
      "0b",
      "000000",
      "0_1",
      "1__1",
      "1____3____5____7"
    )

    forAll(testCases) { (s: String) =>
      TInteger.parser.parseAll(s) shouldBe Symbol("isLeft")
    }

    // Valid for parser but not valid as integer
    TInteger.parser.parseAll(s"1${Long.MaxValue}") shouldBe Symbol("isRight")
    TInteger.parser.parseAll(s"1${Long.MaxValue}").flatMap(_.value) shouldBe Symbol("isLeft")
  }
}

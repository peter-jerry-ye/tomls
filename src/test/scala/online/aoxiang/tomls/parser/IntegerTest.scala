package online.aoxiang.tomls.parser

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
      PInteger.parser.parseAll(s).map(_.value) should be(Right(n))
    }

    // hex, oct, bin without underscore
    forAll { (n: Long) =>
      PInteger.parser.parseAll(s"${n}").map(_.value) should be(Right(n))
      whenever(n >= 0) {
        PInteger.parser.parseAll(s"${n}").map(_.value) should be(Right(n))
        PInteger.parser.parseAll(s"0x${n.toHexString}").map(_.value) should be(Right(n))
        PInteger.parser.parseAll(s"0o${n.toOctalString}").map(_.value) should be(Right(n))
        PInteger.parser.parseAll(s"0b${n.toBinaryString}").map(_.value) should be(Right(n))
      }
    }

    // hex, oct, bin with underscore
    PInteger.parser.parseAll("0x0_d_e_a_d_b_e_e_f").map(_.value) should be(
      Right(
        java.lang.Long.parseLong("deadbeef", 16)
      )
    )
    PInteger.parser.parseAll("0o0_123_45_67").map(_.value) should be(Right(java.lang.Long.parseLong("1234567", 8)))
    PInteger.parser.parseAll("0o7_55").map(_.value) should be(Right(java.lang.Long.parseLong("755", 8)))
    PInteger.parser.parseAll("0b1_10_101_1_0").map(_.value) should be(
      Right(java.lang.Long.parseLong("11010110", 2))
    )
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
      "1____3____5____7",
      "0x_deadbeef",
      "0o_11",
      s"1${Long.MaxValue}"
    )

    forAll(testCases) { (s: String) =>
      PInteger.parser.parseAll(s) shouldBe Symbol("isLeft")
    }
  }
}

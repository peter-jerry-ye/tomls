package online.aoxiang.tomls.ast

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class FloatTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid float") {
    val testCases = Table[String, Double](
      ("input", "expected float"),

      // Cases in official document
      ("+1.0", 1.0),
      ("3.1415", 3.1415),
      ("-0.01", -0.01),
      ("5e+22", 5e22),
      ("1e06", 1e06),
      ("-2E-2", -2e-2),
      ("6.626e-34", 6.626e-34),
      ("224_617.445_991_228", 224617.445991228),
      ("inf", Double.PositiveInfinity),
      ("+inf", Double.PositiveInfinity),
      ("-inf", Double.NegativeInfinity)
    )

    forAll(testCases) { (s: String, f: Double) =>
      TFloat.parser.parseAll(s).flatMap(_.value) should be(Right(f))
    }

    val nanCases = Table(
      "input",
      "nan",
      "+nan",
      "-nan"
    )

  }

  test("Parse invalid float") {
    val testCases = Table(
      "input",

      // Cases in official document
      ".7",
      "7.",
      "3.e+20"
    )

    forAll(testCases) { (s: String) => TFloat.parser.parseAll(s) shouldBe Symbol("isLeft") }
  }
}

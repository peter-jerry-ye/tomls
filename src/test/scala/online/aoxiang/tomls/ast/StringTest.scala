package online.aoxiang.tomls.ast

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid basic strings") {
    // Cases in official document
    TString.basicStringParser.parseAll(raw""""我是一个字符串。\"你可以把我引起来\"。姓名\tJos\u00E9\n位置\t旧金山。"""") should be(
      Right(BasicString(s"我是一个字符串。\"你可以把我引起来\"。姓名\tJos\u00E9\n位置\t旧金山。"))
    )

    // Unicode case
    TString.basicStringParser.parseAll(raw""""\u0041 \U0001F353"""") should be(Right(BasicString("A \uD83C\uDF53")))
  }

  test("Parse valid literal strings") {
    // Cases in official document
    val testCases = Table(
      "input",
      raw"C:\Users\nodejs\templates",
      raw"""\\ServerX\admin$$\system32\""",
      raw"""Tom "Dubs" Preston-Werner""",
      raw"""<\i\c*\s*>"""
    )
    forAll(testCases) { s =>
      TString.literalStringParser.parseAll(raw"'${s}'") should be(Right(LiteralString(s)))
    }
  }
}

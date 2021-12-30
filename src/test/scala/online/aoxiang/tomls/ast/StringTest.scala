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
}

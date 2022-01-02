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

  test("Parse valid multiline basic strings") {
    // Cases in official document
    TString.mlBasicStringParser.parseAll(s"""\"\"\"
    |Roses are red
    |Violets are blue\"\"\"""".stripMargin) should be(
      Right(MLBasicString(s"Roses are red${util.Properties.lineSeparator}Violets are blue"))
    )
    TString.mlBasicStringParser.parseAll(s"""\"\"\"
    |The quick brown \\
    |
    |
    |  fox jumps over \\
    |    the lazy dog.\"\"\"""".stripMargin) should be(
      Right(MLBasicString("The quick brown fox jumps over the lazy dog."))
    )
    val testCases =
      Table(
        ("input", "expected"),
        (raw"""这有两个引号：""。够简单。""", raw"""这有两个引号：""。够简单。"""),
        (raw"""这有三个引号：""\"。""", "这有三个引号：\"\"\"。"),
        ("这有十五个引号：\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"\"\"\\\"。", "这有十五个引号：\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"。"),
        (raw""""这，"她说，"只是个无意义的条款。"""", raw""""这，"她说，"只是个无意义的条款。"""")
      )
    forAll(testCases) { (input: String, expected: String) =>
      TString.mlBasicStringParser.parseAll(s"\"\"\"${input}\"\"\"") should be(Right(MLBasicString(expected)))
    }
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

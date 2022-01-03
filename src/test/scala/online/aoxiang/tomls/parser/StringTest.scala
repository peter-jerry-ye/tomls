package online.aoxiang.tomls.parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class StringTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid basic strings") {
    // Cases in official document
    PString.basicStringParser.parseAll(raw""""我是一个字符串。\"你可以把我引起来\"。姓名\tJos\u00E9\n位置\t旧金山。"""") should be(
      Right(BasicString(s"我是一个字符串。\"你可以把我引起来\"。姓名\tJos\u00E9\n位置\t旧金山。"))
    )

    // Unicode case
    PString.basicStringParser.parseAll(raw""""\u0041 \U0001F353"""") should be(Right(BasicString("A \uD83C\uDF53")))
  }

  test("Parse valid multiline basic strings") {
    // Cases in official document
    PString.mlBasicStringParser.parseAll(s"""\"\"\"
    |Roses are red
    |Violets are blue\"\"\"""".stripMargin) should be(
      Right(MLBasicString(s"Roses are red${util.Properties.lineSeparator}Violets are blue"))
    )
    PString.mlBasicStringParser.parseAll(s"""\"\"\"
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
      PString.mlBasicStringParser.parseAll(s"\"\"\"${input}\"\"\"") should be(Right(MLBasicString(expected)))
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
      PString.literalStringParser.parseAll(raw"'${s}'") should be(Right(LiteralString(s)))
    }
  }

  test("Parse valid multiline literal strings") {
    // Cases in official document
    val testCases = Table(
      "input",
      raw"I [dw]on't need \d{2} apples",
      raw"""
      |原始字符串中的
      |第一个换行被剔除了。
      |   所有其它空白
      |   都保留了。""".stripMargin,
      "这有十五个引号：\"\"\"\"\"\"\"\"\"\"\"\"\"\"\"",
      "'那，'她说，'仍然没有意义。'"
    )
    forAll(testCases) { s =>
      PString.mlLiteralStringParser.parseAll(s"'''${s}'''") should be(Right(MLLiteralString(s.stripLeading)))
    }
  }
}

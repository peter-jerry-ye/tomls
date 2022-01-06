package online.aoxiang.tomls.parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.Inside
import org.scalatest.PartialFunctionValues
import cats.data.Chain

class ArrayTest extends AnyFunSuite with Matchers with Inside with PartialFunctionValues with ScalaCheckPropertyChecks {
  test("Parse arrays with same type") {
    inside(PValue.parser.parseAll("[ 1, 2, 3 ]")) { case Right(InlineArray(array)) =>
      array should be(Chain(TInteger(1), TInteger(2), TInteger(3)))
    }
    inside(PValue.parser.parseAll("[ \"red\", \"yellow\", \"green\" ]")) { case Right(InlineArray(array)) =>
      array should be(Chain(TString("red"), TString("yellow"), TString("green")))
    }

    inside(PValue.parser.parseAll("[ [ 1, 2 ], [3, 4, 5] ]")) { case Right(InlineArray(array)) =>
      array should be(
        Chain(InlineArray(Chain(TInteger(1), TInteger(2))), InlineArray(Chain(TInteger(3), TInteger(4), TInteger(5))))
      )

    }
    inside(PValue.parser.parseAll("[ [ 1, 2 ], [\"a\", \"b\", \"c\"] ]")) { case Right(InlineArray(array)) =>
      array should be(
        Chain(
          InlineArray(Chain(TInteger(1), TInteger(2))),
          InlineArray(Chain(TString("a"), TString("b"), TString("c")))
        )
      )

    }
    inside(PValue.parser.parseAll("[ \"all\", 'strings', \"\"\"are the same\"\"\", '''type''' ]")) {
      case Right(InlineArray(array)) =>
        array should be(Chain(TString("all"), TString("strings"), TString("are the same"), TString("type")))
    }
  }

  test("Parse arrays with mixed types") {
    inside(PValue.parser.parseAll("[ 0.1, 0.2, 0.5, 1, 2, 5 ]")) { case Right(InlineArray(array)) =>
      array should be(Chain(TFloat(0.1), TFloat(0.2), TFloat(0.5), TInteger(1), TInteger(2), TInteger(5)))
    }
    inside(
      PValue.parser
        .parseAll("""[
          |  "Foo Bar <foo@example.com>",
          |  { name = "Baz Qux", email = "bazqux@example.com", url = "https://example.com/bazqux" }
          |]""".stripMargin)
    ) { case Right(InlineArray(array)) =>
      array should be(
        Chain(
          TString("Foo Bar <foo@example.com>"),
          InlineTable(
            Map(
              "name" -> TString("Baz Qux"),
              "email" -> TString("bazqux@example.com"),
              "url" -> TString("https://example.com/bazqux")
            )
          )
        )
      )
    }
  }

  test("Parse arrays with multiline") {
    inside(
      PValue.parser
        .parseAll("""[
        |  1, 2, 3
        |]""".stripMargin)
    ) { case Right(InlineArray(array)) => array should be(Chain(TInteger(1), TInteger(2), TInteger(3))) }
    inside(
      PValue.parser
        .parseAll("""[
        |  1,
        |  2, # 这是可以的
        |]""".stripMargin)
    ) { case Right(InlineArray(array)) => array should be(Chain(TInteger(1), TInteger(2))) }
  }

  test("Parse inline table") {
    inside(PValue.parser.parseAll("{ first = \"Tom\", last = \"Preston-Werner\" }")) {
      case Right(InlineTable(table)) => {
        table valueAt "first" should be(TString("Tom"))
        table valueAt "last" should be(TString("Preston-Werner"))
      }
    }
    inside(PValue.parser.parseAll("{ x = 1, y = 2 }")) {
      case Right(InlineTable(table)) => {
        table valueAt "x" should be(TInteger(1))
        table valueAt "y" should be(TInteger(2))
      }
    }
    inside(PValue.parser.parseAll("{ type.name = \"pug\" }")) {
      case Right(InlineTable(table)) => {
        table valueAt "type" should be(IntermediateTable(Map("name" -> TString("pug"))))
      }
    }
  }

  test("Parse invalid inline table") {
    inside(PValue.parser.parseAll("{ x = 1, y.z = 2, y = {w = 2} }")) { case Left(_) => }
  }
}

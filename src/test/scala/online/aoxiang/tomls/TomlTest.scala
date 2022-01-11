package online.aoxiang.tomls

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatest.Inside
import cats.parse.Parser
import cats.parse.Parser.Expectation.FailWith

class TomlTest extends AnyFunSuite with Matchers with Inside {
  test("API test") {
    inside(Toml.parser.parseAll("""# 这是一个全行注释
        |key = "value"  # 这是一个行末注释
        |another = "# 这不是一个注释"""".stripMargin)) { case Right(t) =>
      t should be(TObject(Map("key" -> TString("value"), "another" -> TString("# 这不是一个注释"))))
    }
  }
  test("Ill formed") {
    inside(Toml.parser.parseAll("""
        |key = # 非法
        """.stripMargin)) {
      case Left(Parser.Error(failedAtOffset, expected)) => {
        failedAtOffset should be(7)
      }
    }
  }
  test("Illegal format") {
    inside(Toml.parser.parseAll("""
        |name = "Tom"
        |name = "Pradyun"
        """.stripMargin)) {
      case Left(Parser.Error(failedAtOffset, expected)) => {
        expected.head should be(FailWith(39, "Can't modify a defined value at: name"))
      }
    }
  }
  test("Show") {
    inside(Toml.parser.parseAll("""
        |key = "value"
        |[owner]
        |name = "Regina Dogman"
        |member_since = 1999-08-04
        |[[fruits]]
        |name = "apple"
        |
        |[fruits.physical]
        |color = "red"
        |shape = "round"
        |
        |[[fruits.varieties]]
        |name = "red delicious"
        |
        |[[fruits.varieties]]
        |name = "granny smith"
        |
        |[[fruits]]
        |name = "banana"
        |
        |[[fruits.varieties]]
        |name = "plantain"
        |""".stripMargin)) { case Right(toml) =>
      Toml.parser.parseAll(toml.show) should be(Right(toml))
    }
  }
}

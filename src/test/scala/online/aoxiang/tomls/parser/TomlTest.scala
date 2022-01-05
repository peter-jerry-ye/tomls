package online.aoxiang.tomls.parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalatest.Inside
import org.scalatest.PartialFunctionValues
import online.aoxiang.tomls.ast._
import java.time.LocalDate

class TomlTest extends AnyFunSuite with Matchers with Inside with PartialFunctionValues with ScalaCheckPropertyChecks {
  test("Parse empty or non empty toml file") {
    PToml.parser.parseAll("# 这是一个全行注释") shouldBe (Symbol("isRight"))
    PToml.parser.parseAll(" ") shouldBe (Symbol("isRight"))
    PToml.parser.parseAll("") shouldBe (Symbol("isRight"))
    PToml.parser.parseAll("""# 这是一个全行注释
        |key = "value"  # 这是一个行末注释
        |another = "# 这不是一个注释"""".stripMargin) shouldBe (Symbol("isRight"))
  }

  test("Parse ill-formed toml file") {
    val illegalKeyValue = Table("illegal input", "key = # 非法", "first = \"Tom\" last = \"Preston-Werner\" # 非法")
    forAll(illegalKeyValue) { s => PToml.parser.parseAll(s) shouldBe (Symbol("isLeft")) }
  }

  test("Parse valid toml file") {
    inside(PToml.parser.parseAll("""
        |# 这是一个全行注释
        |key = "value"  # 这是一个行末注释
        |another = "# 这不是一个注释"
        |""".stripMargin)) {
      case Right(IntermediateTable(table)) => {
        table.valueAt("key") should be(TString("value"))
        table.valueAt("another") should be(TString("# 这不是一个注释"))
      }
    }
    inside(PToml.parser.parseAll("""
        |name = "Orange"
        |physical.color = "orange"
        |physical.shape = "round"
        |site."google.com" = true
        |""".stripMargin)) {
      case (Right(IntermediateTable(table))) => {
        table.valueAt("name") should be(TString("Orange"))
        inside(table.valueAt("physical")) {
          case IntermediateTable(t2) => {
            t2.valueAt("color") should be(TString("orange"))
            t2.valueAt("shape") should be(TString("round"))
          }
        }
        inside(table.valueAt("site")) {
          case IntermediateTable(t2) => {
            t2.valueAt("google.com") should be(TBoolean(true))
          }
        }
      }
    }
    inside(PToml.parser.parseAll("""
        |fruit.name = "banana"     # 这是最佳实践
        |fruit. color = "yellow"    # 等同于 fruit.color
        |fruit . flavor = "banana"   # 等同于 fruit.flavor
        |""".stripMargin)) {
      case Right(IntermediateTable(table)) => {
        inside(table.valueAt("fruit")) {
          case IntermediateTable(t2) => {
            t2.valueAt("name") should be(TString("banana"))
            t2.valueAt("color") should be(TString("yellow"))
            t2.valueAt("flavor") should be(TString("banana"))
          }
        }
      }
    }
    inside(PToml.parser.parseAll("""
        |# 这使“fruit”键作为表存在。
        |fruit.apple.smooth = true
        |
        |# 所以接下来你可以像中这样对“fruit”表添加内容：
        |fruit.orange = 2
        |""".stripMargin)) {
      case Right(IntermediateTable(table)) => {
        inside(table.valueAt("fruit")) {
          case IntermediateTable(t2) => {
            t2.valueAt("orange") should be(TInteger(2))
            inside(t2.valueAt("apple")) { case IntermediateTable(t3) =>
              t3.valueAt("smooth") should be(TBoolean(true))
            }
          }
        }
      }
    }
    inside(PToml.parser.parseAll("""
        |# 合法但不鼓励
        |
        |apple.type = "水果"
        |orange.type = "水果"
        |
        |apple.skin = "薄"
        |orange.skin = "厚"
        |
        |apple.color = "红"
        |orange.color = "橙"
        """.stripMargin)) {
      case Right(IntermediateTable(table)) => {
        inside(table.valueAt("apple")) {
          case IntermediateTable(apple) => {
            apple.valueAt("type") should be(TString("水果"))
            apple.valueAt("skin") should be(TString("薄"))
            apple.valueAt("color") should be(TString("红"))
          }
        }
        inside(table.valueAt("orange")) {
          case IntermediateTable(orange) => {
            orange.valueAt("type") should be(TString("水果"))
            orange.valueAt("skin") should be(TString("厚"))
            orange.valueAt("color") should be(TString("橙"))
          }
        }
      }
    }
    inside(PToml.parser.parseAll("""
        |[table-1]
        |key1 = "some string"
        |key2 = 123
        |
        |[table-2]
        |key1 = "another string"
        |key2 = 456
        """.stripMargin)) {
      case Right(IntermediateTable(root)) => {
        inside(root.valueAt("table-1")) {
          case StandardTable(t1) => {
            t1.valueAt("key1") should be(TString("some string"))
            t1.valueAt("key2") should be(TInteger(123))
          }
        }
        inside(root.valueAt("table-2")) {
          case StandardTable(t2) => {
            t2.valueAt("key1") should be(TString("another string"))
            t2.valueAt("key2") should be(TInteger(456))
          }
        }
      }
    }
    inside(PToml.parser.parseAll("""
        |[dog."tater.man"]
        |type.name = "pug"
        """.stripMargin)) { case Right(IntermediateTable(root)) =>
      inside(root.valueAt("dog")) { case IntermediateTable(dog) =>
        inside(dog.valueAt("tater.man")) { case StandardTable(taterman) =>
          inside(taterman.valueAt("type")) { case IntermediateTable(t) =>
            t.valueAt("name") should be(TString("pug"))
          }
        }
      }
    }
    inside(PToml.parser.parseAll("""
        |# 顶层表开始。
        |name = "Fido"
        |breed = "pug"
        |
        |# 顶层表结束。
        |[owner]
        |name = "Regina Dogman"
        |member_since = 1999-08-04
        """.stripMargin)) {
      case Right(IntermediateTable(root)) => {
        root.valueAt("name") should be(TString("Fido"))
        root.valueAt("breed") should be(TString("pug"))
        inside(root.valueAt("owner")) {
          case StandardTable(owner) => {
            owner.valueAt("name") should be(TString("Regina Dogman"))
            owner.valueAt("member_since") should be(TLocalDate(LocalDate.of(1999, 8, 4)))
          }
        }
      }
    }
  }

  test("Parse invalid toml file") {
    val invalid = Table(
      "invalid cases",
      """
        |name = "Tom"
        |name = "Pradyun"
      """.stripMargin,
      """
      |spelling = "favorite"
      |"spelling" = "favourite"
      """.stripMargin,
      """
      |fruit.apple = 1
      |fruit.apple.smooth = true
      """.stripMargin,
      """
      |[fruit]
      |apple = "红"
      |
      |[fruit]
      |orange = "橙"
      """.stripMargin,
      """
      |[fruit]
      |apple = "红"
      |
      |[fruit.apple]
      |texture = "光滑"
      """.stripMargin
    )
    forAll(invalid) { s =>
      PToml.parser.parseAll(s) shouldBe (Symbol("isLeft"))
    }
  }
}

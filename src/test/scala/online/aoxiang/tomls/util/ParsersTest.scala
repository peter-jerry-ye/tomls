package online.aoxiang.tomls.util

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import org.scalacheck.Gen

class ParsersTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("non-ascii parser") {
    Parsers.non_ascii.parseAll("ɑ") should be(Right("\u0251"))
    Parsers.non_ascii.parseAll("γ") should be(Right("\u03b3"))
    Parsers.non_ascii.parseAll("句") should be(Right("\uf906"))
    Parsers.non_ascii.parseAll("１") should be(Right("\uff11"))
    Parsers.non_ascii.parseAll("\uffff") should be(Right("\uffff"))
    Parsers.non_ascii.parseAll("𐀀") should be(Right("\ud800\udc00")) // 0x10000
    Parsers.non_ascii.parseAll("🍓") should be(Right("\uD83C\uDF53")) // 0x1f353
  }

  test("newline parser") {
    Parsers.newline.parseAll("\n") shouldBe (Symbol("isRight"))
    Parsers.newline.parseAll("\r\n") shouldBe (Symbol("isRight"))
  }

  test("escaped parser") {
    val escapedGen = Gen.oneOf('"', '\\', 'b', 'f', 'n', 'r', 't')
    forAll(escapedGen)((ch: Char) =>
      Parsers.escaped.parseAll(raw"\${ch}") should be(Right(raw"\${ch}".translateEscapes))
    )

    // Unicode
    Parsers.escaped.parseAll("\\U0001f353") should be(Right("🍓"))
    Parsers.escaped.parseAll("\\uffff") should be(Right("\uffff"))
  }

  test("ws parser") {
    val wsGen = Gen.listOf(Gen.oneOf('\t', ' '))
    forAll(wsGen)((chs: List[Char]) => Parsers.ws.parseAll(chs.mkString) shouldBe (Symbol("isRight")))
  }
}

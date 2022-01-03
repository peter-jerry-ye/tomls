package online.aoxiang.tomls.parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks
import cats.data.NonEmptyList

class KeyTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid key") {
    // Cases from official document
    val bareKeys = Table("bare key", "key", "bare_key", "bare-key", "1234")
    forAll(bareKeys) { s =>
      PKey.parser.parseAll(s) should be(Right(NonEmptyList(s, Nil)))
    }

    val quotedKeys = Table(
      ("quoted key", "expected"),
      (raw""""127.0.0.1"""", "127.0.0.1"),
      (raw""""character encoding"""", "character encoding"),
      (raw""""ʎǝʞ"""", "ʎǝʞ"),
      (raw"'key2'", "key2"),
      (raw"""'quoted "value"'""", "quoted \"value\""),
      (raw"""""""", ""),
      ("''", "")
    )
    forAll(quotedKeys) { (s, expected) =>
      PKey.parser.parseAll(s) should be(Right(NonEmptyList(expected, Nil)))
    }

    val dottedKeys = Table(
      ("dotted key", "expected"),
      ("name", NonEmptyList("name", Nil)),
      ("physical.color", NonEmptyList("physical", List("color"))),
      ("physical.shape", NonEmptyList("physical", List("shape"))),
      ("site.\"google.com\"", NonEmptyList("site", List("google.com"))),
      ("fruit.name", NonEmptyList("fruit", List("name"))),
      ("fruit. color", NonEmptyList("fruit", List("color"))),
      ("fruit . flavor", NonEmptyList("fruit", List("flavor"))),
      ("3.14159", NonEmptyList("3", List("14159")))
    )
    forAll(dottedKeys) { (s, expected) =>
      PKey.parser.parseAll(s) should be(Right(expected))
    }

    // Additional tests
    PKey.parser.parse("asdf ") should be(Right((" ", NonEmptyList("asdf", List.empty))))
  }
}

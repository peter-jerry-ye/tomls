package online.aoxiang.tomls

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class ParserTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid integers") {
    val testCases = Table[String, Long](
      ("s", "n"),
      // Some corner cases
      (s"${Long.MaxValue}", Long.MaxValue),
      (s"${Long.MinValue}", Long.MinValue),

      // Cases in official document
      ("+99", 99),
      ("42", 42),
      ("0", 0),
      ("-17", -17),
      ("1_000", 1_000),
      ("5_349_221", 5_349_221),
      ("53_49_221", 53_49_221),
      ("1_2_3_4_5", 1_2_3_4_5),
      ("0xDEADBEEF", 0xdeadbeefL),
      ("0xdeadbeef", 0xdeadbeefL),
      ("0xdead_beef", 0xdead_beefL),
      ("0o01234567", java.lang.Long.parseLong("1234567", 8)),
      ("0o755", java.lang.Long.parseLong("755", 8)),
      ("0b11010110", java.lang.Long.parseLong("11010110", 2))
    )

    forAll(testCases) { (s: String, n: Long) =>
      TomlsParser.integer.parseAll(s).flatMap(_.value) should be(Right(n))
    }

    // hex, oct, bin without underscore
    forAll { (n: Long) =>
      TomlsParser.integer.parseAll(s"${n}").flatMap(_.value) should be(Right(n))
      whenever(n >= 0) {
        TomlsParser.integer.parseAll(s"${n}").flatMap(_.value) should be(Right(n))
        TomlsParser.integer.parseAll(s"0x${n.toHexString}").flatMap(_.value) should be(Right(n))
        TomlsParser.integer.parseAll(s"0o${n.toOctalString}").flatMap(_.value) should be(Right(n))
        TomlsParser.integer.parseAll(s"0b${n.toBinaryString}").flatMap(_.value) should be(Right(n))
      }
    }
  }

  test("Parse invalid integers") {
    val testCases = Table(
      "wrong inputs",
      "asdf",
      "0X123",
      "0B456",
      "0O11",
      "0o",
      "0x",
      "0b",
      "000000",
      "0_1",
      "1__1",
      "1____3____5____7"
    )

    forAll(testCases) { (s: String) =>
      TomlsParser.integer.parseAll(s) shouldBe Symbol("isLeft")
    }

    // Valid for parser but not valid as integer
    TomlsParser.integer.parseAll(s"1${Long.MaxValue}") shouldBe Symbol("isRight")
    TomlsParser.integer.parseAll(s"1${Long.MaxValue}").flatMap(_.value) shouldBe Symbol("isLeft")
  }

  test("Parse valid boolean") {
    TomlsParser.boolean.parseAll("true") should be(Right(true))
    TomlsParser.boolean.parseAll("false") should be(Right(false))
  }

  test("Parse valid time") {
    import java.time._
    val testCases = Table[String, LocalDate | LocalTime | LocalDateTime | ZonedDateTime](
      ("input", "time"),
      // Cases in official document
      ("1979-05-27T07:32:00Z", ZonedDateTime.of(1979, 5, 27, 7, 32, 0, 0, ZoneOffset.UTC)),
      (
        "1979-05-27T00:32:00-07:00",
        ZonedDateTime.of(1979, 5, 27, 0, 32, 0, 0, ZoneOffset.ofHours(-7))
      ),
      ("1979-05-27T00:32:00.999999-07:00", ZonedDateTime.of(1979, 5, 27, 0, 32, 0, 999999000, ZoneOffset.ofHours(-7))),
      (
        "1979-05-27 07:32:00Z",
        ZonedDateTime.of(1979, 5, 27, 7, 32, 0, 0, ZoneOffset.UTC)
      ),
      ("1979-05-27T07:32:00", LocalDateTime.of(1979, 5, 27, 7, 32, 0)),
      ("1979-05-27T00:32:00.999999", LocalDateTime.of(1979, 5, 27, 0, 32, 0, 999999000)),
      ("1979-05-27", LocalDate.of(1979, 5, 27)),
      ("07:32:00", LocalTime.of(7, 32, 0)),
      ("00:32:00.999999", LocalTime.of(0, 32, 0, 999999000))
    )
    forAll(testCases) { (s: String, t: LocalDate | LocalTime | LocalDateTime | ZonedDateTime) =>
      TomlsParser.time.parseAll(s).flatMap(_.value) should be(Right(t))
    }
  }

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
      TomlsParser.float.parseAll(s).flatMap(_.value) should be(Right(f))
    }

    val nanCases = Table(
      "input",
      "nan",
      "+nan",
      "-nan"
    )

    forAll(nanCases) { (s: String) =>
      {
        TomlsParser.float.parseAll(s).flatMap(_.value).map(_.isNaN) should be(Right(true))
      }
    }
  }

  test("Parse invalid float") {
    val testCases = Table(
      "input",

      // Cases in official document
      ".7",
      "7.",
      "3.e+20"
    )

    forAll(testCases) { (s: String) => TomlsParser.float.parseAll(s) shouldBe Symbol("isLeft") }
  }
}

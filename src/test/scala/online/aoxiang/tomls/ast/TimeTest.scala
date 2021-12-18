package online.aoxiang.tomls.ast

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class TimeTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
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
      TTime.parser.parseAll(s).flatMap(_.value) should be(Right(t))
    }
  }
}

package online.aoxiang.tomls.parser

import java.time.{LocalDate, LocalTime, LocalDateTime, ZonedDateTime, DateTimeException, ZoneOffset}
import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234._
import online.aoxiang.tomls.ast.TOffsetDateTime
import online.aoxiang.tomls.ast.TLocalDateTime
import online.aoxiang.tomls.ast.TLocalDate
import online.aoxiang.tomls.ast.TLocalTime

sealed trait PTime {
  def value: Either[DateTimeException, LocalDate | LocalTime | LocalDateTime | ZonedDateTime]
}

object PTime {
  val parser: Parser[TOffsetDateTime | TLocalDateTime | TLocalDate | TLocalTime] = {
    val date_full_year = digit.rep(4, 4)
    val date_month = digit.rep(2, 2)
    val date_mday = digit.rep(2, 2)
    val time_hour = digit.rep(2, 2)
    val time_minute = digit.rep(2, 2)
    val time_second = digit.rep(2, 2)
    val time_delim = sp | Parser.ignoreCaseChar('T')
    val time_secfrac = Parser.char('.') *> digit.rep(1)
    val time_numoffset = (Parser.char('+') | Parser.char('-')) ~ time_hour ~ Parser.char(':') ~ time_minute
    val time_offset = (Parser.ignoreCaseChar('Z') | time_numoffset).string

    val partial_time =
      (time_hour ~ Parser.char(':') ~ time_minute ~ Parser.char(':') ~ time_second ~ time_secfrac.?).string
    val full_date = (date_full_year ~ Parser.char('-') ~ date_month ~ Parser.char('-') ~ date_mday).string
    val full_time = partial_time ~ time_offset

    val local_date: Parser[TLocalDate] = full_date
      .map(PLocalDate(_))
      .flatMap(date =>
        date.value
          .fold(e => Parser.failWith(s"Not a valid date: ${e.getLocalizedMessage}"), v => Parser.pure(TLocalDate(v)))
      )
    val local_time: Parser[TLocalTime] = partial_time
      .map(PLocalTime(_))
      .flatMap(time =>
        time.value
          .fold(e => Parser.failWith(s"Not a valid time: ${e.getLocalizedMessage}"), v => Parser.pure(TLocalTime(v)))
      )
    val offset_date_time: Parser[TOffsetDateTime] = (full_date ~ time_delim ~ full_time)
      .map((date, time) => PZonedDateTime(PLocalDateTime(PLocalDate(date._1), PLocalTime(time._1)), time._2))
      .flatMap(dateTime =>
        dateTime.value.fold(
          e => Parser.failWith(s"Not a valid offset datetime: ${e.getLocalizedMessage}"),
          v => Parser.pure(TOffsetDateTime(v))
        )
      )
    val local_date_time: Parser[TLocalDateTime] =
      (full_date ~ time_delim ~ partial_time)
        .map((date, time) => PLocalDateTime(PLocalDate(date._1), PLocalTime(time)))
        .flatMap(dateTime =>
          dateTime.value.fold(
            e => Parser.failWith(s"Not a valid local datetime: ${e.getLocalizedMessage}"),
            v => Parser.pure(TLocalDateTime(v))
          )
        )

    (offset_date_time.backtrack | local_date_time.backtrack | local_date.backtrack | local_time)
      .asInstanceOf[Parser[TOffsetDateTime | TLocalDateTime | TLocalDate | TLocalTime]]
  }
}

case class PLocalDate(date: String) extends PTime {
  override def value: Either[DateTimeException, LocalDate] =
    Either.catchOnly[DateTimeException](LocalDate.parse(date))
}
case class PLocalTime(time: String) extends PTime {
  override def value: Either[DateTimeException, LocalTime] =
    Either.catchOnly[DateTimeException](LocalTime.parse(time))
}
case class PLocalDateTime(date: PLocalDate, time: PLocalTime) extends PTime {
  override def value: Either[DateTimeException, LocalDateTime] = for {
    d <- date.value
    t <- time.value
  } yield LocalDateTime.of(d, t)
}
case class PZonedDateTime(dateTime: PLocalDateTime, offset: String) extends PTime {
  override def value: Either[DateTimeException, ZonedDateTime] = for {
    dt <- dateTime.value
    zonedDT <- Either.catchOnly[DateTimeException](ZonedDateTime.of(dt, ZoneOffset.of(offset)))
  } yield zonedDT
}

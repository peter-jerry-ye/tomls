package online.aoxiang.tomls.ast

import java.time.{LocalDate, LocalTime, LocalDateTime, ZonedDateTime, DateTimeException, ZoneOffset}
import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234._

sealed trait TTime {
  def value: Either[DateTimeException, LocalDate | LocalTime | LocalDateTime | ZonedDateTime]
}

object TTime {
  val parser: Parser[TTime] = {
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

    val local_date: Parser[TLocalDate] = full_date.map(TLocalDate(_))
    val local_time: Parser[TLocalTime] = partial_time.map(TLocalTime(_))
    val offset_date_time: Parser[TZonedDateTime] = (full_date ~ time_delim ~ full_time).map((date, time) =>
      TZonedDateTime(TLocalDateTime(TLocalDate(date._1), TLocalTime(time._1)), time._2)
    )
    val local_date_time: Parser[TLocalDateTime] =
      (full_date ~ time_delim ~ partial_time).map((date, time) => TLocalDateTime(TLocalDate(date._1), TLocalTime(time)))

    offset_date_time.backtrack | local_date_time.backtrack | local_date.backtrack | local_time
  }
}

case class TLocalDate(date: String) extends TTime {
  override def value: Either[DateTimeException, LocalDate] =
    Either.catchOnly[DateTimeException](LocalDate.parse(date))
}
case class TLocalTime(time: String) extends TTime {
  override def value: Either[DateTimeException, LocalTime] =
    Either.catchOnly[DateTimeException](LocalTime.parse(time))
}
case class TLocalDateTime(date: TLocalDate, time: TLocalTime) extends TTime {
  override def value: Either[DateTimeException, LocalDateTime] = for {
    d <- date.value
    t <- time.value
  } yield LocalDateTime.of(d, t)
}
case class TZonedDateTime(dateTime: TLocalDateTime, offset: String) extends TTime {
  override def value: Either[DateTimeException, ZonedDateTime] = for {
    dt <- dateTime.value
    zonedDT <- Either.catchOnly[DateTimeException](ZonedDateTime.of(dt, ZoneOffset.of(offset)))
  } yield zonedDT
}

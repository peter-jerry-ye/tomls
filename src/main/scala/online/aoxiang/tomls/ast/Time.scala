package online.aoxiang.tomls.ast

import java.time.{LocalDate, LocalTime, LocalDateTime, ZonedDateTime, DateTimeException, ZoneOffset}
import cats.implicits._

sealed trait TTime {
  def value: Either[DateTimeException, LocalDate | LocalTime | LocalDateTime | ZonedDateTime]
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

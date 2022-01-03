package online.aoxiang.tomls.ast

import java.time.{ZonedDateTime, LocalDateTime, LocalDate, LocalTime}
import cats.FlatMap

sealed trait TValue
case class TString(value: String) extends TValue
case class TInteger(value: Long) extends TValue
case class TFloat(value: Double) extends TValue
case class TBoolean(value: Boolean) extends TValue
case class TOffsetDateTime(value: ZonedDateTime) extends TValue
case class TLocalDateTime(value: LocalDateTime) extends TValue
case class TLocalDate(value: LocalDate) extends TValue
case class TLocalTime(value: LocalTime) extends TValue
case class TArray(value: Seq[TValue]) extends TValue
case class TTable(value: Map[String, TValue]) extends TValue
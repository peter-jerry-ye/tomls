package online.aoxiang.tomls.parser

import java.time.{ZonedDateTime, LocalDateTime, LocalDate, LocalTime}
import cats.FlatMap
import cats.data._

sealed trait TValue
case class TString(value: String) extends TValue
case class TInteger(value: Long) extends TValue
case class TFloat(value: Double) extends TValue
case class TBoolean(value: Boolean) extends TValue
case class TOffsetDateTime(value: ZonedDateTime) extends TValue
case class TLocalDateTime(value: LocalDateTime) extends TValue
case class TLocalDate(value: LocalDate) extends TValue
case class TLocalTime(value: LocalTime) extends TValue
sealed trait TArray extends TValue {
  def value: Chain[TValue]
}
sealed trait TTable extends TValue {
  def value: Map[String, TValue]
  def isDefined: Boolean
  def isClosed: Boolean
}

case class InlineArray(value: Chain[TValue]) extends TArray
case class TableArray(tables: NonEmptyChain[StandardTable]) extends TArray {
  def value = tables.toChain
}

case class InlineTable(value: Map[String, TValue]) extends TTable {
  override def isDefined = true
  override def isClosed = true
}
case class IntermediateTable(value: Map[String, TValue]) extends TTable {
  override def isDefined = false
  override def isClosed = false
}
case class StandardTable(value: Map[String, TValue]) extends TTable {
  override def isDefined = true
  override def isClosed = false
}

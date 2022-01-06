package online.aoxiang.tomls

import cats.parse.Parser0
import online.aoxiang.tomls.parser._
import cats.Eval
import cats.implicits._
import java.time._

sealed trait Toml
case class TString(value: String) extends Toml
case class TDouble(value: Double) extends Toml
case class TLong(value: Long) extends Toml
case class TBool(value: Boolean) extends Toml
case class TZonedDateTime(value: ZonedDateTime) extends Toml
case class TLocalDateTime(value: LocalDateTime) extends Toml
case class TLocalDate(value: LocalDate) extends Toml
case class TLocalTime(value: LocalTime) extends Toml
case class TObject(value: Map[String, Toml]) extends Toml
case class TArray(value: List[Toml]) extends Toml

object Toml {
  val parser: Parser0[TObject] = {
    def astToToml(v: TValue): Eval[Toml] = v match {
      case TInteger(i)                                    => Eval.now(TLong(i))
      case TBoolean(b)                                    => Eval.now(TBool(b))
      case TFloat(f)                                      => Eval.now(TDouble(f))
      case online.aoxiang.tomls.parser.TString(s)         => Eval.now(TString(s))
      case online.aoxiang.tomls.parser.TOffsetDateTime(t) => Eval.now(TZonedDateTime(t))
      case online.aoxiang.tomls.parser.TLocalDateTime(t)  => Eval.now(TLocalDateTime(t))
      case online.aoxiang.tomls.parser.TLocalDate(t)      => Eval.now(TLocalDate(t))
      case online.aoxiang.tomls.parser.TLocalTime(t)      => Eval.now(TLocalTime(t))
      case array: online.aoxiang.tomls.parser.TArray =>
        array.value.toList.traverse(t => Eval.defer(astToToml(t))).map(TArray(_))
      case table: TTable =>
        table.value.toList.traverse(p => Eval.defer(astToToml(p._2)).map(p._1 -> _)).map(_.toMap).map(TObject(_))
    }
    PToml.parser.map(astToToml(_).value.asInstanceOf[TObject])
  }
}

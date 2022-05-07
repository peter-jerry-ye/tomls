package online.aoxiang.tomls

import cats.parse.Parser0
import online.aoxiang.tomls.parser.PToml
import cats.Eval
import cats.Show
import cats.implicits._
import java.time._
import java.time.format.DateTimeFormatter
import cats.data.Chain

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
    import online.aoxiang.tomls.{parser => ast}
    def astToToml(v: ast.TValue): Eval[Toml] = v match {
      case ast.TInteger(i)        => Eval.now(TLong(i))
      case ast.TBoolean(b)        => Eval.now(TBool(b))
      case ast.TFloat(f)          => Eval.now(TDouble(f))
      case ast.TString(s)         => Eval.now(TString(s))
      case ast.TOffsetDateTime(t) => Eval.now(TZonedDateTime(t))
      case ast.TLocalDateTime(t)  => Eval.now(TLocalDateTime(t))
      case ast.TLocalDate(t)      => Eval.now(TLocalDate(t))
      case ast.TLocalTime(t)      => Eval.now(TLocalTime(t))
      case array: ast.TArray =>
        array.value.toList.traverse(t => Eval.defer(astToToml(t))).map(TArray(_))
      case table: ast.TTable =>
        table.value.toList.traverse(p => Eval.defer(astToToml(p._2)).map(p._1 -> _)).map(_.toMap).map(TObject(_))
    }
    PToml.parser.map(astToToml(_).value.asInstanceOf[TObject])
  }

  given Show[TLong] with {
    def show(value: TLong) = value.value.show
  }

  given Show[TBool] with {
    def show(value: TBool) = value.value.show
  }

  given Show[TDouble] with {
    def show(value: TDouble) = value.value.show
  }

  given Show[TZonedDateTime] with {
    def show(value: TZonedDateTime) = value.value.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
  }

  given Show[TLocalDateTime] with {
    def show(value: TLocalDateTime) = value.value.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
  }

  given Show[TLocalDate] with {
    def show(value: TLocalDate) = value.value.format(DateTimeFormatter.ISO_LOCAL_DATE)
  }

  given Show[TLocalTime] with {
    def show(value: TLocalTime) = value.value.format(DateTimeFormatter.ISO_LOCAL_TIME)
  }

  given Show[TString] with {
    def show(value: TString) = {
      val escaped = value.value.flatMap(c =>
        c match {
          case '\"' => "\\\""
          case '\\' => raw"\\"
          case '\b' => raw"\b"
          case '\f' => raw"\f"
          case '\n' => raw"\n"
          case '\r' => raw"\r"
          case '\t' => raw"\t"
          case _    => s"${c}"
        }
      )
      s"\"${escaped}\""
    }
  }

  private def keyToString(key: String): String = {
    TString(key).show
  }

  extension (t: TObject)
    def show: String = {
      def inlineObjectToString(o: TObject): String = {
        o.value
          .map((key, toml) =>
            toml match {
              case v: TLong          => s"${keyToString(key)} = ${v.show}"
              case v: TBool          => s"${keyToString(key)} = ${v.show}"
              case v: TDouble        => s"${keyToString(key)} = ${v.show}"
              case v: TZonedDateTime => s"${keyToString(key)} = ${v.show}"
              case v: TLocalDateTime => s"${keyToString(key)} = ${v.show}"
              case v: TLocalDate     => s"${keyToString(key)} = ${v.show}"
              case v: TLocalTime     => s"${keyToString(key)} = ${v.show}"
              case v: TString        => s"${keyToString(key)} = ${v.show}"
              case o: TObject        => s"${keyToString(key)} = ${inlineObjectToString(o)}"
              case r: TArray         => s"${keyToString(key)} = ${inlineArrayToString(r)}"
            }
          )
          .mkString("{", ",", "}")
      }

      def inlineArrayToString(a: TArray): String = {
        a.value
          .map(toml =>
            toml match {
              case v: TLong          => v.show
              case v: TBool          => v.show
              case v: TDouble        => v.show
              case v: TZonedDateTime => v.show
              case v: TLocalDateTime => v.show
              case v: TLocalDate     => v.show
              case v: TLocalTime     => v.show
              case v: TString        => v.show
              case o: TObject        => inlineObjectToString(o)
              case r: TArray         => inlineArrayToString(r)
            }
          )
          .mkString("[", ",", "]")
      }
      t.value
        .map((key, toml) =>
          toml match {
            case v: TLong          => s"${keyToString(key)} = ${v.show}"
            case v: TBool          => s"${keyToString(key)} = ${v.show}"
            case v: TDouble        => s"${keyToString(key)} = ${v.show}"
            case v: TZonedDateTime => s"${keyToString(key)} = ${v.show}"
            case v: TLocalDateTime => s"${keyToString(key)} = ${v.show}"
            case v: TLocalDate     => s"${keyToString(key)} = ${v.show}"
            case v: TLocalTime     => s"${keyToString(key)} = ${v.show}"
            case v: TString        => s"${keyToString(key)} = ${v.show}"
            case o: TObject        => s"${keyToString(key)} = ${inlineObjectToString(o)}"
            case r: TArray         => s"${keyToString(key)} = ${inlineArrayToString(r)}"
          }
        )
        .mkString(util.Properties.lineSeparator)
    }
}

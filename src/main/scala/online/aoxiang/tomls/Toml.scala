package online.aoxiang.tomls

import cats.parse.Parser0
import online.aoxiang.tomls.parser.PToml
import cats.Eval
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

  extension (t: TObject)
    def show: String = {
      def valueToString(
          v: TLong | TBool | TDouble | TZonedDateTime | TLocalDateTime | TLocalDate | TLocalTime
      ): String = v match {
        case TLong(i)          => i.show
        case TBool(b)          => b.show
        case TDouble(f)        => f.show
        case TZonedDateTime(t) => t.format(DateTimeFormatter.ISO_OFFSET_DATE_TIME)
        case TLocalDateTime(t) => t.format(DateTimeFormatter.ISO_LOCAL_DATE_TIME)
        case TLocalDate(t)     => t.format(DateTimeFormatter.ISO_LOCAL_DATE)
        case TLocalTime(t)     => t.format(DateTimeFormatter.ISO_LOCAL_TIME)
      }
      def strToString(v: TString): String = {
        val escaped = v.value.flatMap(c =>
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
      def inlineObjectToString(o: TObject): String = {
        o.value
          .map((key, toml) =>
            toml match {
              case v: (TLong | TBool | TDouble | TZonedDateTime | TLocalDateTime | TLocalDate | TLocalTime) =>
                s"${key} = ${valueToString(v)}"
              case s: TString => s"${key} = ${strToString(s)}"
              case o: TObject => s"${key} = ${inlineObjectToString(o)}"
              case r: TArray  => s"${key} = ${inlineArrayToString(r)}"
            }
          )
          .mkString("{", ",", "}")
      }

      def inlineArrayToString(a: TArray): String = {
        a.value
          .map(toml =>
            toml match {
              case v: (TLong | TBool | TDouble | TZonedDateTime | TLocalDateTime | TLocalDate | TLocalTime) =>
                valueToString(v)
              case s: TString => strToString(s)
              case o: TObject => inlineObjectToString(o)
              case r: TArray  => inlineArrayToString(r)
            }
          )
          .mkString("[", ",", "]")
      }
      t.value
        .map((key, toml) =>
          toml match {
            case v: (TLong | TBool | TDouble | TZonedDateTime | TLocalDateTime | TLocalDate | TLocalTime) =>
              s"${key} = ${valueToString(v)}"
            case s: TString => s"${key} = ${strToString(s)}"
            case o: TObject => s"${key} = ${inlineObjectToString(o)}"
            case r: TArray  => s"${key} = ${inlineArrayToString(r)}"
          }
        )
        .mkString(util.Properties.lineSeparator)
    }
}

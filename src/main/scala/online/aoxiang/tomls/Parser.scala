package online.aoxiang.tomls

import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.parse.Parser0
import cats.data.NonEmptyList
import online.aoxiang.tomls.ast._

object TomlsParser {

  /** parse integer from -2^63 to 2^63-1
    */
  def integer: Parser[TInteger] = {
    import ast._
    val underscore = Parser.char('_')
    val digit1_9 = Parser.charIn(for { i <- 1 to 9 } yield (i + 48).toChar)
    val digit0_7 = Parser.charIn(for { i <- 0 to 7 } yield (i + 48).toChar)
    val digit0_1 = Parser.charIn(for { i <- 0 to 1 } yield (i + 48).toChar)

    val hex_prefix = Parser.string("0x")
    val oct_prefix = Parser.string("0o")
    val bin_prefix = Parser.string("0b")

    val unsigned_dec_int = ((digit1_9 ~ (digit | underscore *> digit).rep(1)).backtrack | digit)
      .map(s =>
        s match {
          case c: Char => s"${c}"
          case (hd, tl): (Char, NonEmptyList[Char]) => {
            (hd :: tl).toList.mkString
          }
        }
      )

    val dec_int = (Parser.charIn('+', '-').?.with1 ~ unsigned_dec_int)
      .map(DecInteger(_, _))

    val hex_int =
      (hex_prefix *> hexdig ~ (hexdig | (underscore *> hexdig)).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(HexInteger(_))
    val oct_int =
      (oct_prefix *> digit0_7 ~ (digit0_7 | (underscore *> digit0_7)).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(OctInteger(_))
    val bin_int =
      (bin_prefix *> digit0_1 ~ (digit0_1 | (underscore *> digit0_1)).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(BinInteger(_))

    hex_int | oct_int | bin_int | dec_int
  }

  /** parse boolean
    */
  def boolean: Parser[TBoolean] = {
    (Parser.string("true") | Parser.string("false")).string.map(java.lang.Boolean.parseBoolean(_))
  }

  /** parse time
    */
  def time: Parser[TTime] = {
    import ast._
    val date_full_year = digit.repExactlyAs[List[_]](4).string.map(Integer.parseInt(_))
    def digits_2 = digit.repExactlyAs[List[_]](2).string.map(Integer.parseInt(_))
    val date_month = digits_2
    val date_mday = digits_2
    val time_hour = digits_2
    val time_minute = digits_2
    val time_second = digits_2
    val time_delim = sp | Parser.ignoreCaseChar('T')
    val time_secfrac = Parser.char('.') *> digit.rep(1)
    val time_numoffset = (Parser.char('+') | Parser.char('-')) *> time_hour ~ Parser.char(':') ~ time_minute
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

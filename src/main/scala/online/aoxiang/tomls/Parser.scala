package online.aoxiang.tomls

import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.parse.Parser0
import cats.data.NonEmptyList

object TomlsParser {

  /**
   * parse integer from -2^63 to 2^63-1
   */
  // TODO: handle exception
  // TODO: test invalid cases
  def integer: Parser[Long] = {
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
          case c: Char => (c.toInt - 48).toLong
          case (hd, tl): (Char, NonEmptyList[Char]) => {
            val str = (hd :: tl).toList.mkString
            if (str == s"${Long.MinValue}".drop(1)) Long.MinValue
            else java.lang.Long.parseLong(str)
          }
        }
      )

    val dec_int = (Parser.charIn('+', '-').?.with1 ~ unsigned_dec_int)
      .map((sign, num) =>
        sign match {
          case Some('-') => -num
          case _         => num
        }
      )

    val hex_int =
      (hex_prefix *> hexdig ~ (hexdig | (underscore *> hexdig)).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(str => java.lang.Long.parseLong(str, 16))
    val oct_int =
      (oct_prefix *> digit0_7 ~ (digit0_7 | (underscore *> digit0_7)).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(str => java.lang.Long.parseLong(str, 8))
    val bin_int =
      (bin_prefix *> digit0_1 ~ (digit0_1 | (underscore *> digit0_1)).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(str => java.lang.Long.parseLong(str, 2))

    hex_int | oct_int | bin_int | dec_int
  }

}

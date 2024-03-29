package online.aoxiang.tomls.parser

import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.data.NonEmptyList

sealed trait PInteger {
  def value: Either[NumberFormatException, Long]
}

object PInteger {
  val parser: Parser[TInteger] = {
    val underscore = Parser.char('_')
    val digit1_9 = Parser.charIn('1' to '9')
    val digit0_7 = Parser.charIn('0' to '7')
    val digit0_1 = Parser.charIn('0' to '1')

    val hex_prefix = Parser.string("0x")
    val oct_prefix = Parser.string("0o")
    val bin_prefix = Parser.string("0b")

    val unsigned_dec_int = ((digit1_9 ~ ((underscore.?.with1 *> digit).rep)).backtrack | digit)
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
      (hex_prefix *> hexdig.repSep(underscore.?))
        .map(_.toList.mkString)
        .map(HexInteger(_))
    val oct_int =
      (oct_prefix *> digit0_7.repSep(underscore.?))
        .map(_.toList.mkString)
        .map(OctInteger(_))
    val bin_int =
      (bin_prefix *> digit0_1.repSep(underscore.?))
        .map(_.toList.mkString)
        .map(BinInteger(_))

    (hex_int | oct_int | bin_int | dec_int).flatMap(n =>
      n.value.fold(e => Parser.failWith(s"Not an integer: ${e.getLocalizedMessage}"), v => Parser.pure(TInteger(v)))
    )
  }
}

case class BinInteger(str: String) extends PInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 2))
}
case class OctInteger(str: String) extends PInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 8))
}
case class HexInteger(str: String) extends PInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 16))
}
case class DecInteger(sign: Option[Char], str: String) extends PInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(s"${sign.getOrElse("")}${str}"))
}

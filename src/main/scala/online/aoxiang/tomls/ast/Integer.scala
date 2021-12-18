package online.aoxiang.tomls.ast
import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.data.NonEmptyList

sealed trait TInteger {
  def value: Either[NumberFormatException, Long]
}

object TInteger {
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
      (hex_prefix *> hexdig ~ (underscore.?.with1 *> hexdig).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(HexInteger(_))
    val oct_int =
      (oct_prefix *> digit0_7 ~ (underscore.?.with1 *> digit0_7).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(OctInteger(_))
    val bin_int =
      (bin_prefix *> digit0_1 ~ (underscore.?.with1 *> digit0_1).rep0)
        .map((hd, tl) => (hd :: tl).mkString)
        .map(BinInteger(_))

    hex_int | oct_int | bin_int | dec_int
  }
}

case class BinInteger(str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 2))
}
case class OctInteger(str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 8))
}
case class HexInteger(str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 16))
}
case class DecInteger(sign: Option[Char], str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(s"${sign.getOrElse("")}${str}"))
}

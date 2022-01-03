package online.aoxiang.tomls.parser

import cats.implicits._
import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.data.NonEmptyList
import online.aoxiang.tomls.ast.TFloat

sealed trait PFloat {
  def value: Either[NumberFormatException, Double]
}

object PFloat {
  val parser: Parser[TFloat] = {
    val inf = Parser.string("inf")
    val nan = Parser.string("nan")
    val specialFloat =
      (Parser.charIn('+', '-').?.with1 ~ (inf | nan).string).map((sign, float) => SpecialFloat(sign, float))

    val underscore = Parser.char('_')
    val zero_prefixable_int = (digit ~ (underscore.?.with1 *> digit).rep0).map(_ :: _).map(_.mkString)

    val float_exp_part = ((Parser.charIn('+', '-').?.with1) ~ zero_prefixable_int)
      .map((sign, float) => s"${sign.getOrElse("")}${float.mkString}")
    val exp = Parser.ignoreCaseChar('e') *> float_exp_part

    val frac = Parser.char('.') *> zero_prefixable_int

    val digit1_9 = Parser.charIn('1' to '9')
    val unsigned_dec_int = ((digit1_9 ~ ((underscore.?.with1 *> digit).rep)).backtrack | digit)
      .map(s =>
        s match {
          case c: Char => s"${c}"
          case (hd, tl): (Char, NonEmptyList[Char]) => {
            (hd :: tl).toList.mkString
          }
        }
      )
    val dec_int = (Parser.charIn('+', '-').?.with1 ~ unsigned_dec_int).map((ch, v) => s"${ch.getOrElse("")}${v}")
    val float_int_part = dec_int

    val normalFloat: Parser[PFloat] = (float_int_part ~ (exp.backtrack | (frac ~ exp.?)))
      .map((int_part, frac) =>
        frac match {
          case e: String                        => NormalFloat(int_part, None, Some(e))
          case (f, e): (String, Option[String]) => NormalFloat(int_part, Some(f), e)
        }
      )

    (normalFloat.backtrack | specialFloat).flatMap(f =>
      f.value.fold(
        e => Parser.failWith(s"Not a valid double: ${e.getLocalizedMessage}"),
        v => Parser.pure(TFloat(v))
      )
    )
  }
}

case class NormalFloat(intPart: String, frac: Option[String], exp: Option[String]) extends PFloat {
  override def value = Either.catchOnly[NumberFormatException] {
    exp match {
      case Some(e) => java.lang.Double.parseDouble(s"${intPart}.${frac.getOrElse("")}E${e}")
      case None    => java.lang.Double.parseDouble(s"${intPart}.${frac.getOrElse("")}")
    }
  }
}

case class SpecialFloat(sign: Option[Char], float: String) extends PFloat {
  override def value = (sign, float) match {
    case (Some('-'), "inf") => Right(Double.NegativeInfinity)
    case (Some('+'), "inf") => Right(Double.PositiveInfinity)
    case (None, "inf")      => Right(Double.PositiveInfinity)
    case (Some('-'), "nan") => Right(Double.NaN)
    case (Some('+'), "nan") => Right(Double.NaN)
    case (None, "nan")      => Right(Double.NaN)
    case _                  => Left(new NumberFormatException())
  }
}

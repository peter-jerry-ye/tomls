package online.aoxiang.tomls.ast

import online.aoxiang.tomls.util.Parsers._

import cats.parse.Parser
import cats.parse.Parser0
import cats.parse.Rfc5234._
import cats.data.NonEmptyList

trait TString {
  def value: String
}

object TString {
  val basicStringParser: Parser[BasicString] = {
    val basic_unescaped: Parser[String] =
      (wsp | Parser.char('!')
        | Parser.charIn(0x23.toChar to 0x5b.toChar)
        | Parser.charIn(0x5d.toChar to 0x7e.toChar)
        | non_ascii).string

    val basic_char: Parser[String] = basic_unescaped | escaped
    (basic_char.rep0.with1 surroundedBy Parser.char('"')).map(_.mkString).map(BasicString(_))
  }

  val mlBasicStringParser: Parser[MLBasicString] = {
    val delim_start = Parser.string("\"\"\"")
    val delim_end = Parser.string("\"\"\"") *> Parser.char('\"').rep0(0, 2).string

    val mlb_unescaped = (wsp | Parser.char('!')
      | Parser.charIn(0x23.toChar to 0x5b.toChar)
      | Parser.charIn(0x5d.toChar to 0x7e.toChar)
      | non_ascii).string
    val mlb_escaped_nl = (Parser.char('\\') <* ws ~ newline ~ (wsp | newline).rep0)
    val mlb_quotes = Parser.char('\"').rep(1, 2).string
    val mlb_char = mlb_unescaped | escaped
    val mlb_content = mlb_char.backtrack | newline.map(_ => util.Properties.lineSeparator) | mlb_escaped_nl.map(_ => "")

    val ml_basic_body = mlb_content.rep0 ~ (mlb_quotes ~ mlb_content.rep).backtrack.rep0
    (delim_start *> newline.? *> ml_basic_body ~ delim_end)
      .map({ case ((l1, l2), quotes) =>
        MLBasicString(
          s"${l1.mkString}${l2.flatMap({ case (hd, tl) => (hd :: tl).toList }).mkString}${quotes}"
        )
      })
  }

  val literalStringParser: Parser[LiteralString] = {
    val literal_char =
      (htab
        | Parser.charIn(0x20.toChar to 0x26.toChar)
        | Parser.charIn(0x28.toChar to 0x7e.toChar)
        | non_ascii).string

    (literal_char.rep0.with1 surroundedBy Parser.char('\'')).map(_.mkString).map(LiteralString(_))
  }

  val mlLiteralStringParser: Parser[MLLiteralString] = {
    val delim_start = Parser.string("'''")
    val delim_end = Parser.string("'''") *> Parser.char('\'').rep0(0, 2).string

    val mll_quotes = Parser.char('\'').rep(1, 2).string
    val mll_char = (htab
      | Parser.charIn(0x20.toChar to 0x26.toChar)
      | Parser.charIn(0x28.toChar to 0x7e.toChar)
      | non_ascii).string
    val mll_content = mll_char | newline.string

    val ml_literal_body = mll_content.rep0 ~ (mll_quotes ~ mll_content.rep).backtrack.rep0
    (delim_start *> newline.? *> ml_literal_body ~ delim_end)
      .map({ case ((l1, l2), quotes) =>
        MLLiteralString(
          s"${l1.mkString}${l2.flatMap({ case (hd, tl) => (hd :: tl).toList }).mkString}${quotes}"
        )
      })
  }
}

case class BasicString(value: String) extends TString

case class LiteralString(value: String) extends TString

case class MLBasicString(value: String) extends TString

case class MLLiteralString(value: String) extends TString

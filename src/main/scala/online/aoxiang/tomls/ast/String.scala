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
  val basicStringParser: Parser0[BasicString] = {
    val basic_unescaped: Parser[String] =
      (wsp | Parser.char('!')
        | Parser.charIn(0x23.toChar to 0x5b.toChar)
        | Parser.charIn(0x5d.toChar to 0x7e.toChar)
        | non_ascii).string

    val basic_char: Parser[String] = basic_unescaped | escaped
    basic_char.rep0.surroundedBy(Parser.char('"')).map(_.mkString).map(BasicString(_))
  }

  val literalStringParser: Parser0[LiteralString] = {
    val literal_char =
      (htab
        | Parser.charIn(0x20.toChar to 0x26.toChar)
        | Parser.charIn(0x28.toChar to 0x7e.toChar)
        | non_ascii).string

    literal_char.rep0.surroundedBy(Parser.char('\'')).map(_.mkString).map(LiteralString(_))
  }
}

case class BasicString(value: String) extends TString

case class LiteralString(value: String) extends TString

case class MLBasicString(value: String) extends TString

case class MLLiteralString(value: String) extends TString

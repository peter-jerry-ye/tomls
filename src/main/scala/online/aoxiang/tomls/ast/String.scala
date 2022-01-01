package online.aoxiang.tomls.ast

import online.aoxiang.tomls.util.Parsers

import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.data.NonEmptyList
import cats.parse.Parser0

trait TString {
  def value: String
}

object TString {
  val basicStringParser: Parser0[BasicString] = {
    val escape_seq_char =
      Parser.charIn('"', '\\', 'b', 'f', 'n', 'r', 't')
        | (Parser.char('u') ~ hexdig.rep(4, 4))
        | (Parser.char('U') ~ hexdig.rep(8, 8))
    val escaped = (Parser.char('\\') *> escape_seq_char).map({
      case ch: Char => StringContext.processEscapes(s"\\${ch}")
      case (_, chs): (Unit, NonEmptyList[Char]) =>
        if (chs.length == 4) StringContext.processEscapes(raw"\u${chs.toList.mkString}")
        // UTF 16
        else {
          val code = Integer.parseInt(chs.toList.mkString, 16)
          val binary = (code - 0x10000).toBinaryString
          val (high, low) =
            s"${List.fill(20 - binary.length)('0').mkString}${binary}".splitAt(10)
          StringContext.processEscapes(
            raw"\u${(0xd800 + Integer.parseInt(high, 2)).toHexString}\u${(0xdc00 + Integer.parseInt(low, 2)).toHexString}"
          )
        }
    })

    val basic_unescaped: Parser[String] =
      (wsp | Parser.char('!')
        | Parser.charIn(0x23.toChar to 0x5b.toChar)
        | Parser.charIn(0x5d.toChar to 0x7e.toChar)
        | Parsers.non_ascii).string

    val basic_char: Parser[String] = basic_unescaped | escaped
    basic_char.rep0.surroundedBy(Parser.char('"')).map(_.mkString).map(BasicString(_))
  }

  val literalStringParser: Parser0[LiteralString] = {
    val literal_char =
      (htab
        | Parser.charIn(0x20.toChar to 0x26.toChar)
        | Parser.charIn(0x28.toChar to 0x7e.toChar)
        | Parsers.non_ascii).string

    literal_char.rep0.surroundedBy(Parser.char('\'')).map(_.mkString).map(LiteralString(_))
  }
}

case class BasicString(value: String) extends TString

case class LiteralString(value: String) extends TString

case class MLBasicString(value: String) extends TString

case class MLLiteralString(value: String) extends TString

package online.aoxiang.tomls.util

import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.data.NonEmptyList

object Parsers {
  val non_ascii: Parser[String] =
    (Parser.charIn(0x80.toChar to 0xd7ff.toChar)
      | Parser.charIn(0xe000.toChar to 0xffff.toChar)
      | (Parser.charIn(0xd800.toChar to 0xdbff.toChar) ~ Parser.charIn(0xdc00.toChar to 0xdfff.toChar))).string

  val newline = lf | crlf
  val ws = wsp.rep0
  val escaped = {
    val escape_seq_char = Parser.charIn('"', '\\', 'b', 'f', 'n', 'r', 't')
      | (Parser.char('u') ~ hexdig.rep(4, 4))
      | (Parser.char('U') ~ hexdig.rep(8, 8))

    (Parser.char('\\') *> escape_seq_char).map({
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
  }

}

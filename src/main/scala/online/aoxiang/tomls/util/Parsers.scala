package online.aoxiang.tomls.util

import cats.parse.Parser

object Parsers {
  val non_ascii: Parser[String] =
    (Parser.charIn(0x80.toChar to 0xd7ff.toChar)
      | Parser.charIn(0xe000.toChar to 0xffff.toChar)
      | (Parser.charIn(0xd800.toChar to 0xdbff.toChar) ~ Parser.charIn(0xdc00.toChar to 0xdfff.toChar))).string
}

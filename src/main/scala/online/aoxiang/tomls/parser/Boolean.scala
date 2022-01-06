package online.aoxiang.tomls.parser

import cats.parse.Parser

object PBoolean {
  val parser: Parser[TBoolean] = {
    (Parser.string("true").as(TBoolean(true)) | Parser.string("false").as(TBoolean(false)))
  }
}

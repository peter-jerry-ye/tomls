package online.aoxiang.tomls.parser

import cats.parse.Parser

type TBoolean = Boolean

object TBoolean {
  val parser: Parser[TBoolean] = {
    (Parser.string("true") | Parser.string("false")).string.map(java.lang.Boolean.parseBoolean(_))
  }
}

package online.aoxiang.tomls.parser

import cats.parse.Parser
import Parsers._

object PComment {
  val parser: Parser[Unit] = Parser.char('#') <* non_eol.rep0
}

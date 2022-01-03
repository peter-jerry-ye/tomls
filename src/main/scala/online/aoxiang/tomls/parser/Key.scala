package online.aoxiang.tomls.parser

import cats.parse.Parser
import cats.parse.Rfc5234._
import Parsers._
import cats.data.NonEmptyList

object PKey {
  val parser: Parser[NonEmptyList[String]] = {
    val quoted_key: Parser[String] = PString.basicStringParser.map(_.value) | PString.literalStringParser.map(_.value)
    val unquoted_key: Parser[String] = (alpha | digit | Parser.charIn("_-")).rep.map(_.toList.mkString)
    val simple_key: Parser[String] = quoted_key | unquoted_key

    val dot_sep = Parser.char('.').surroundedBy(ws)
    val dotted_key: Parser[NonEmptyList[String]] = simple_key.repSep(dot_sep.backtrack)

    dotted_key.backtrack
  }
}

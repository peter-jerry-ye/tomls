package online.aoxiang.tomls

import cats.parse.Parser
import cats.parse.Rfc5234._
import cats.parse.Parser0
import cats.data.NonEmptyList
import online.aoxiang.tomls.ast._

object TomlsParser {

  /** parse integer from -2^63 to 2^63-1
    */
  def integer: Parser[TInteger] = TInteger.parser

  /** parse float
    */
  def float: Parser[TFloat] = TFloat.parser

  /** parse boolean
    */
  def boolean: Parser[TBoolean] = TBoolean.parser

  /** parse time
    */
  def time: Parser[TTime] = TTime.parser
}

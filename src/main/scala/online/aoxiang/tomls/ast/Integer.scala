package online.aoxiang.tomls.ast
import cats.implicits._

sealed trait TInteger {
  def value: Either[NumberFormatException, Long]
}

case class BinInteger(str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 2))
}
case class OctInteger(str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 8))
}
case class HexInteger(str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(str, 16))
}
case class DecInteger(sign: Option[Char], str: String) extends TInteger {
  def value = Either.catchOnly[NumberFormatException](java.lang.Long.parseLong(s"${sign.getOrElse("")}${str}"))
}

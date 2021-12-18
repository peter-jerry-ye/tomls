package online.aoxiang.tomls.ast

import cats.implicits._

sealed trait TFloat {
  def value: Either[NumberFormatException, Double]
}

case class NormalFloat(intPart: String, frac: Option[String], exp: Option[String]) extends TFloat {
  override def value = Either.catchOnly[NumberFormatException] {
    exp match {
      case Some(e) => java.lang.Double.parseDouble(s"${intPart}.${frac.getOrElse("")}E${e}")
      case None    => java.lang.Double.parseDouble(s"${intPart}.${frac.getOrElse("")}")
    }
  }
}

case class SpecialFloat(sign: Option[Char], float: String) extends TFloat {
  override def value = (sign, float) match {
    case (Some('-'), "inf") => Right(Double.NegativeInfinity)
    case (Some('+'), "inf") => Right(Double.PositiveInfinity)
    case (None, "inf")      => Right(Double.PositiveInfinity)
    case (Some('-'), "nan") => Right(Double.NaN)
    case (Some('+'), "nan") => Right(Double.NaN)
    case (None, "nan")      => Right(Double.NaN)
    case _                  => Left(new NumberFormatException())
  }
}

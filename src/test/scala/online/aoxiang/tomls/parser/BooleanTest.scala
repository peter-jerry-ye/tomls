package online.aoxiang.tomls.parser

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BooleanTest extends AnyFunSuite with Matchers {
  test("Parse valid boolean") {
    PBoolean.parser.parseAll("true").map(_.value) should be(Right(true))
    PBoolean.parser.parseAll("false").map(_.value) should be(Right(false))
  }
}

package online.aoxiang.tomls.ast

import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
import org.scalatestplus.scalacheck.ScalaCheckPropertyChecks

class BooleanTest extends AnyFunSuite with Matchers with ScalaCheckPropertyChecks {
  test("Parse valid boolean") {
    TBoolean.parser.parseAll("true") should be(Right(true))
    TBoolean.parser.parseAll("false") should be(Right(false))
  }
}

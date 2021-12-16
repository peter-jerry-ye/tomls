import org.scalatest.funsuite.AnyFunSuite
import org.scalatest.matchers.should.Matchers
class Test1 extends AnyFunSuite with Matchers {
  test("Test") {
    msg should be("I was compiled by Scala 3. :)")
  }
}

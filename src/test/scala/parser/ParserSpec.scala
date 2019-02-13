package parser

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "parser" should {
    "return success" in {
      Parser("").parse("aaa") shouldBe a[ParseSuccess[_]]
    }
  }
}

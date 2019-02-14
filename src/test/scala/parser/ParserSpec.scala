package parser

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "parser" should {
    "return success" in {
      Parser("a").parse("abb") shouldBe a[ParseSuccess[_]]
      Parser("a").parse("a") shouldBe
        new ParseSuccess[String]("a", "")
      Parser("a").parse("abb") shouldBe
        new ParseSuccess[String]("a", "bb")
    }
    "return false" in {
      Parser("b").parse("aaa") shouldBe a[ParseFailure]
      Parser("bbbb").parse("aaa") shouldBe a[ParseFailure]
      Parser("bbbb").parse("aaa") shouldBe
        new ParseFailure("parse error. expected:bbbb")
    }
  }
}

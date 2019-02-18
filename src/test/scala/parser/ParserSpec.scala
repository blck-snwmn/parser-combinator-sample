package parser

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "parser" should {
    "return success" in {
      Parser("a").parse("abb") shouldBe a[ParseSuccess[_]]
      Parser("a").parse("a") shouldBe
        new ParseSuccess("a", "")
      Parser("a").parse("abb") shouldBe
        new ParseSuccess("a", "bb")
    }
    "return false" in {
      Parser("b").parse("aaa") shouldBe a[ParseFailure]
      Parser("bbbb").parse("aaa") shouldBe a[ParseFailure]
      Parser("bbbb").parse("aaa") shouldBe
        new ParseFailure("parse error. expected:bbbb")
    }
  }
  "many parser" should {
    "no match" in {
      Parser("a").many.parse("b") shouldBe a[ParseSuccess[_]]
      Parser("a").many.parse("b") shouldBe
        new ParseSuccess(List(), "b")
    }

    "one match" in {
      Parser("a").many.parse("a") shouldBe a[ParseSuccess[_]]
      Parser("a").many.parse("a") shouldBe
        new ParseSuccess(List("a"), "")
    }

    "more match" in {
      Parser("a").many.parse("aa") shouldBe a[ParseSuccess[_]]
      Parser("a").many.parse("aa") shouldBe
        new ParseSuccess(List("a", "a"), "")
      Parser("a").many.parse("aab") shouldBe
        new ParseSuccess(List("a", "a"), "b")
    }
  }

  "or parser" should {
    "match first parser" in {
      Parser("a").or(Parser("b")).parse("aa") shouldBe a[ParseSuccess[_]]
      Parser("a").or(Parser("b")).parse("aa") shouldBe
        new ParseSuccess("a", "a")
    }

    "match second parser" in {
      Parser("a").or(Parser("b")).parse("ba") shouldBe a[ParseSuccess[_]]
      Parser("a").or(Parser("b")).parse("ba") shouldBe
        new ParseSuccess("b", "a")
    }

    "no match" in {
      Parser("a").or(Parser("b")).parse("cab") shouldBe a[ParseFailure]
    }
  }

  "seq parser" should {
    "all success" in {
      Parser("a").seq(Parser("b")).parse("ab") shouldBe
        new ParseSuccess(("a", "b"), "")
    }

    "fail first parser" in {
      Parser("a").seq(Parser("b")).parse("ba") shouldBe
        new ParseFailure("parse error. expected:a")
    }

    "fail after parser" in {
      Parser("a").seq(Parser("b")).parse("ac") shouldBe
        new ParseFailure("parse error. expected:b")
    }
  }

  "parser combine" can {
    "or parser and many parser" should {
      "use or*3 -> many" in {
        Parser("a")
          .or(Parser("b"))
          .or(Parser("c"))
          .many
          .parse("cbaabcbca") shouldBe new ParseSuccess(List("c", "b", "a", "a", "b", "c", "b", "c", "a"), "")
      }

      "use many before another parser " in {
        Parser("a")
          .many
          .or(Parser("c").many())
          .parse("ac") shouldBe new ParseSuccess(List("a"), "c")
      }
    }
  }

}

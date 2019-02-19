package parser

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "parser" should {
    "return success" in {
      Parser("a").parse("abb") shouldBe a[ParseSuccess[_]]
      Parser("a").parse("a") shouldBe
        ParseSuccess("a", "")
      Parser("a").parse("abb") shouldBe
        ParseSuccess("a", "bb")
    }
    "return false" in {
      Parser("b").parse("aaa") shouldBe a[ParseFailure]
      Parser("bbbb").parse("aaa") shouldBe a[ParseFailure]
      Parser("bbbb").parse("aaa") shouldBe
        ParseFailure("parse error. expected:bbbb")
    }
  }
  "many parser" should {
    "no match" in {
      Parser("a").many.parse("b") shouldBe a[ParseSuccess[_]]
      Parser("a").many.parse("b") shouldBe
        ParseSuccess(List(), "b")
    }

    "one match" in {
      Parser("a").many.parse("a") shouldBe a[ParseSuccess[_]]
      Parser("a").many.parse("a") shouldBe
        ParseSuccess(List("a"), "")
    }

    "more match" in {
      Parser("a").many.parse("aa") shouldBe a[ParseSuccess[_]]
      Parser("a").many.parse("aa") shouldBe
        ParseSuccess(List("a", "a"), "")
      Parser("a").many.parse("aab") shouldBe
        ParseSuccess(List("a", "a"), "b")
    }
  }

  "or parser" should {
    "match first parser" in {
      Parser("a").or(Parser("b")).parse("aa") shouldBe a[ParseSuccess[_]]
      Parser("a").or(Parser("b")).parse("aa") shouldBe
        ParseSuccess("a", "a")
    }

    "match second parser" in {
      Parser("a").or(Parser("b")).parse("ba") shouldBe a[ParseSuccess[_]]
      Parser("a").or(Parser("b")).parse("ba") shouldBe
        ParseSuccess("b", "a")
    }

    "no match" in {
      Parser("a").or(Parser("b")).parse("cab") shouldBe a[ParseFailure]
    }
  }

  "seq parser" should {
    "all success" in {
      Parser("a").seq(Parser("b")).parse("ab") shouldBe
        ParseSuccess(("a", "b"), "")
    }

    "fail first parser" in {
      Parser("a").seq(Parser("b")).parse("ba") shouldBe
        ParseFailure("parse error. expected:a")
    }

    "fail after parser" in {
      Parser("a").seq(Parser("b")).parse("ac") shouldBe
        ParseFailure("parse error. expected:b")
    }
  }

  "option parser" should {
    "parse success" in {
      Parser("a").option().parse("aa") shouldBe ParseSuccess(Some("a"), "a")
    }

    "parse failure" in {
      Parser("a").option().parse("bb") shouldBe ParseSuccess(None, "bb")
    }
  }

  "parser combine" can {
    "or parser and many parser" should {
      "use or*3 -> many" in {
        Parser("a")
          .or(Parser("b"))
          .or(Parser("c"))
          .many
          .parse("cbaabcbca") shouldBe ParseSuccess(List("c", "b", "a", "a", "b", "c", "b", "c", "a"), "")
      }

      "use many before another parser " in {
        Parser("a")
          .many
          .or(Parser("c").many())
          .parse("ac") shouldBe ParseSuccess(List("a"), "c")
      }
    }
  }

}

package parser

import org.scalatest._

class ParserSpec extends WordSpec with Matchers {
  "parser" should {
    "return success" in {
      Parser("a").parse("a") shouldBe
        ParseSuccess("a", "")
      Parser("a").parse("abb") shouldBe
        ParseSuccess("a", "bb")
    }
    "return false" in {
      Parser("b").parse("aaa") shouldBe
        ParseFailure("parse error. expected:b")
      Parser("bbbb").parse("aaa") shouldBe
        ParseFailure("parse error. expected:bbbb")
    }
  }
  "many parser" should {
    "no match" in {
      Parser("a").many.parse("b") shouldBe
        ParseSuccess(List(), "b")
    }

    "one match" in {
      Parser("a").many.parse("a") shouldBe
        ParseSuccess(List("a"), "")
    }

    "more match" in {
      Parser("a").many.parse("aa") shouldBe
        ParseSuccess(List("a", "a"), "")
      Parser("a").many.parse("aab") shouldBe
        ParseSuccess(List("a", "a"), "b")
    }
  }

  "or parser" should {
    "match first parser" in {
      Parser("a").or(Parser("b")).parse("aa") shouldBe
        ParseSuccess("a", "a")
    }

    "match second parser" in {
      Parser("a").or(Parser("b")).parse("ba") shouldBe
        ParseSuccess("b", "a")
    }

    "no match" in {
      Parser("a").or(Parser("b")).parse("cab") shouldBe
        ParseFailure("parse error. expected:b")
    }
  }

  "seq parser" should {
    "do eager evaluation" in {
      val changedValue = 1
      var changeValue = 0
      val evaluatedFun: String => ParseResult[String] = {
        _ =>
          changeValue = changedValue
          ParseSuccess("", "")
      }
      Parser("a").seq(Parser(evaluatedFun)).parse("aa")
      changeValue shouldBe changedValue
    }
    "delay evaluation" in {
      val expectedValue = 0
      var nonChangeValue = expectedValue
      val nonEvaluatedFun: String => ParseResult[String] = {
        _ =>
          nonChangeValue = 1
          ParseSuccess("", "")
      }
      Parser("b").seq(Parser(nonEvaluatedFun)).parse("aa")
      nonChangeValue shouldBe expectedValue
    }
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

  "map parser" should {
    "parse success" in {
      Parser("a").map(i => s"input is $i").parse("aa") shouldBe ParseSuccess("input is a", "a")
    }
    "parse another type except String" in {
      Parser("12").map(i => i.toInt).parse("123") shouldBe ParseSuccess(12, "3")
    }
    "failure another parser" in {
      Parser("a").map(i => s"input is $i").parse("b") shouldBe ParseFailure("parse error. expected:a")
    }
  }

  "select parser" should {
    "parse success" in {
      Parser.select(Set('a', 'b', 'c')).parse("acb") shouldBe ParseSuccess("a", "cb")
      Parser.selectChar("abc").parse("bac") shouldBe ParseSuccess("b", "ac")
    }
    "parse failure" in {
      Parser.select(Set('a', 'b', 'c')).parse("dabc") shouldBe ParseFailure("parse error. expected in:Set(a, b, c)")
      Parser.selectChar("abc").parse("dabc") shouldBe ParseFailure("parse error. expected in:Set(a, b, c)")
    }

    "empty" in {
      Parser.select(Set.empty[Char]).parse("abc") shouldBe ParseFailure("parse error. expected in:Set()")
      Parser.select(Set.empty[Char]).parse("") shouldBe ParseFailure("parse error. expected in:Set()")
      Parser.selectChar("").parse("abc") shouldBe ParseFailure("parse error. expected in:Set()")
      Parser.selectChar("").parse("") shouldBe ParseFailure("parse error. expected in:Set()")
    }
  }

  "end parser" should {
    "parse success" in {
      Parser("a").end().parse("a") shouldBe ParseSuccess("a", "")
    }

    "parse failure" in {
      Parser("a").end().parse("aa") shouldBe ParseFailure("parse error. unnecessary character at the end: a")
    }
  }
  "mathematical expression" should {
    "calculate" in {
      //とりあえずIntで扱える範囲で
      def calc(l: Int, r: Int, operator: String): Int = {
        operator match {
          case "+" => l + r
          case "-" => l - r
          case _ => throw new Exception("error")
        }
      }

      val numberParser = Parser.selectChar((1 to 9).mkString("")).map(n => n.toInt)
      val operatorParser = Parser.selectChar("-+")

      lazy val expressionParser: Parser[Int] = {
        //カッコつきの式
        lazy val expressionBucketsParser = Parser("(")
          .seq(expressionParser)
          .seq(Parser(")"))
          .map(v => v._1._2)
        //数値またはカッコつきの式
        val atom = numberParser.or(expressionBucketsParser)
        atom
          .seq(operatorParser.seq(atom).many)
          .map(v => {
            v._2.foldLeft(v._1)((a, b) => calc(a, b._2, b._1))
          })
      }

      expressionParser.end.parse("3+(1-2)+2+2-5") shouldBe ParseSuccess(1, "")
      expressionParser.end.parse("(1-2)+2+2-") shouldBe ParseFailure("parse error. unnecessary character at the end: -")
    }
  }
}

package parser

class Parser(val input: String) {
  def parse(target: String): ParseResult = {
    new ParseSuccess[String]("", "")
  }
}

object Parser {
  def apply(input: String): Parser = new Parser(input)
}

abstract class ParseResult

class ParseSuccess[T](val result: T, val next: String) extends ParseResult

class ParseFailure extends ParseResult

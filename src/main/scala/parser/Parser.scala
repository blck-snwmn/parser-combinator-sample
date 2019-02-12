package parser

class Parser(val input: String) {
  def parse(target: String): ParseResult = {
    new ParseSuccess[String]("", "")
  }
}

object Parser {
  def apply(input: String): Parser = new Parser(input)
}

sealed abstract class ParseResult

final class ParseSuccess[T](val result: T, val next: String) extends ParseResult

final class ParseFailure extends ParseResult

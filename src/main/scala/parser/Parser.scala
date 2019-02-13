package parser

class Parser(val input: String) {
  def parse(target: String): ParseResult = {
    target.startsWith(input) match {
      case true => new ParseSuccess[String]("", "")
      case false => new ParseFailure("")
    }
  }
}

object Parser {
  def apply(input: String): Parser = new Parser(input)
}

sealed abstract class ParseResult

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult

final case class ParseFailure(val errorMessage: String) extends ParseResult

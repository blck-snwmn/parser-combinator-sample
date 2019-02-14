package parser

class Parser[T](parser: T => ParseResult) {
  def parse(target: T): ParseResult = {
    parser(target)
  }
}

object Parser {
  def apply[T](parser: T => ParseResult): Parser[T] = new Parser(parser)

  def apply(input: String): Parser[String] = Parser[String]({ target =>
    target.startsWith(input) match {
      case true => new ParseSuccess[String]("", "")
      case false => new ParseFailure("")
    }
  })
}


sealed abstract class ParseResult

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult

final case class ParseFailure(val errorMessage: String) extends ParseResult

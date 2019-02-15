package parser

class Parser[T](parser: String => ParseResult[T]) {
  def parse(target: String): ParseResult[T] = {
    parser(target)
  }
}

object Parser {
  def apply[T](parser: String => ParseResult[T]): Parser[T] = new Parser(parser)

  def apply(input: String): Parser[String] = Parser[String]({ target =>
    target.startsWith(input) match {
      case true => new ParseSuccess[String](input, target.substring(input.length))
      case false => new ParseFailure(s"parse error. expected:$input")
    }
  })
}


sealed abstract class ParseResult[+T]

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult[T]

final case class ParseFailure(val errorMessage: String) extends ParseResult[Nothing]

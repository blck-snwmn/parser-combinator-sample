package parser

import scala.collection.mutable

class Parser[T](parser: String => ParseResult[T]) {
  def parse(target: String): ParseResult[T] = {
    parser(target)
  }

  def many(): Parser[List[T]] = Parser { target =>
    def parseRecursively(result: mutable.ListBuffer[T], next: String): ParseResult[List[T]] = {
      parse(next) match {
        case ParseSuccess(r, n) => {
          result += r
          parseRecursively(result, n)
        }
        case ParseFailure(_) => new ParseSuccess(result.toList, next)
      }
    }

    parseRecursively(mutable.ListBuffer.empty, target)
  }

  def or(parser: Parser[T]): Parser[T] = Parser { target =>
    this.parse(target) match {
      case ParseSuccess(r, n) => new ParseSuccess(r, n)
      case ParseFailure(_) => parser.parse(target)
    }
  }
}

object Parser {
  def apply[T](parser: String => ParseResult[T]): Parser[T] = new Parser(parser)

  def apply(input: String): Parser[String] = Parser[String] { target =>
    target.startsWith(input) match {
      case true => new ParseSuccess[String](input, target.substring(input.length))
      case false => new ParseFailure(s"parse error. expected:$input")
    }
  }

  def many(input: String): Parser[List[String]] = {
    Parser(input).many
  }
}


sealed abstract class ParseResult[+T]

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult[T]

final case class ParseFailure(val errorMessage: String) extends ParseResult[Nothing]

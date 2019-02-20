package parser

import scala.collection.mutable

class Parser[T](parser: String => ParseResult[T]) {
  def parse(target: String): ParseResult[T] = {
    parser(target)
  }

  def option(): Parser[Option[T]] = Parser { target =>
    this.parse(target) match {
      case ParseSuccess(r, n) => ParseSuccess(Some(r), n)
      case ParseFailure(_) => ParseSuccess(None, target)
    }
  }

  def many(): Parser[List[T]] = Parser { target =>
    def parseRecursively(result: mutable.ListBuffer[T], next: String): ParseResult[List[T]] = {
      parse(next) match {
        case ParseSuccess(r, n) => {
          result += r
          parseRecursively(result, n)
        }
        case ParseFailure(_) => ParseSuccess(result.toList, next)
      }
    }

    parseRecursively(mutable.ListBuffer.empty, target)
  }

  def or(parser: Parser[T]): Parser[T] = Parser { target =>
    this.parse(target) match {
      case success@ParseSuccess(_, _) => success
      case ParseFailure(_) => parser.parse(target)
    }
  }

  def seq[U](parser: Parser[U]): Parser[(T, U)] = Parser { target =>
    this.parse(target) match {
      case ParseSuccess(r1, n2) => {
        parser.parse(n2) match {
          case ParseSuccess(r2, n2) => ParseSuccess((r1, r2), n2)
          case failure@ParseFailure(_) => failure
        }
      }
      case failure@ParseFailure(_) => failure
    }
  }

  def map[U](f: T => U): Parser[U] = Parser { target =>
    this.parse(target) match {
      case ParseSuccess(r, n) => ParseSuccess(f(r), n)
      case failure@ParseFailure(_) => failure
    }
  }
}

object Parser {
  def apply[T](parser: String => ParseResult[T]): Parser[T] = new Parser(parser)

  def apply(input: String): Parser[String] = Parser[String] { target =>
    target.startsWith(input) match {
      case true => ParseSuccess[String](input, target.substring(input.length))
      case false => ParseFailure(s"parse error. expected:$input")
    }
  }

  def many(input: String): Parser[List[String]] = {
    Parser(input).many
  }
}

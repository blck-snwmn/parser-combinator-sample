package parser

import scala.collection.mutable

/** Does Parse using args function
  * this class created companion object
  *
  * @param parser parse method called
  * @tparam T for result at ParseResult
  */
class Parser[T](parser: String => ParseResult[T]) {
  /** apply parse function
    *
    * @param target
    * @return ParseResult
    */
  def parse(target: String): ParseResult[T] = {
    parser(target)
  }

  /** return success that wrapped Option
    *
    * @return parser instance
    */
  def option(): Parser[Option[T]] = Parser[Option[T]] {
    target =>
      this.parse(target).fold(
        _ => ParseSuccess(None, target),
        (r, n) => ParseSuccess(Some(r), n)
      )
  }

  /** parse many times until parse fail
    * always success
    *
    * @return always success parser
    */
  def many(): Parser[List[T]] = Parser {
    target =>
      def parseRecursively(result: mutable.ListBuffer[T], next: String): ParseResult[List[T]] = {
        parse(next).fold(
          _ => ParseSuccess(result.toList, next),
          (r, n) => {
            result += r
            parseRecursively(result, n)
          }
        )
      }

      parseRecursively(mutable.ListBuffer.empty, target)
  }

  /** after own parser parse fail, use args parser
    *
    * @param parser
    * @return parser instance
    */
  def or(parser: => Parser[T]): Parser[T] = Parser {
    target =>
      this.parse(target).fold(
        _ => parser.parse(target),
        (r, n) => ParseSuccess(r, n)
      )
  }

  /** after own parse, use args parser
    * if both parser success, return success
    *
    * @param parser
    * @tparam U
    * @return parser instance
    */
  def seq[U](parser: => Parser[U]): Parser[(T, U)] = Parser {
    target =>
      for {
        (r1, n1) <- parse(target)
        (r2, n2) <- parser.parse(n1)
      } yield (r1 -> r2) -> n2
  }

  /** convert parse result
    *
    * @param f user for conversion parse result
    * @tparam U convert result type
    * @return parser instance
    */
  def map[U](f: T => U): Parser[U] = Parser(parse(_).map(f))

  /** end parser.
    * After parse, if next string is empty, return Success.
    *
    * @return parser instance
    */
  def end(): Parser[T] = Parser {
    target =>
      this.parse(target) match {
        case ParseSuccess(_, n) if !n.isEmpty =>
          ParseFailure(s"parse error. unnecessary character at the end: $n")
        case result@_ =>
          result
      }
  }
}

/**
  * Factory for [[parser.Parser]] instance
  */
object Parser {
  def apply[T](parser: String => ParseResult[T]): Parser[T] = new Parser(parser)

  def apply(input: String): Parser[String] = Parser[String] {
    target =>
      if (target.startsWith(input))
        ParseSuccess[String](input, target.substring(input.length))
      else
        ParseFailure(s"parse error. expected:$input")
  }

  /** input match many times
    *
    * @param input
    * @return parser instance
    */
  def many(input: String): Parser[List[String]] = {
    Parser(input).many
  }

  /** first character of input match params
    *
    * @param set
    * @return parser instance
    */
  def select(set: Set[Char]): Parser[String] = Parser {
    target =>
      set.find(c => target.startsWith(c.toString)) match {
        case Some(c) =>
          val str = c.toString
          ParseSuccess(str, target.substring(str.length))
        case None =>
          ParseFailure(s"parse error. expected in:$set")
      }
  }

  /** first character of input match params
    *
    * @param str
    * @return parser instance
    */
  def selectChar(str: String): Parser[String] = select(str.toSet)
}

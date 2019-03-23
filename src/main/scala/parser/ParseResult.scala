package parser

sealed abstract class ParseResult[+T] {
  def map[U](f: T => U): ParseResult[U] = {
    this match {
      case ParseSuccess(r, n) =>
        ParseSuccess(f(r), n)
      case _ => this.asInstanceOf[ParseResult[U]]
    }
  }
}

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult[T]

final case class ParseFailure(val errorMessage: String) extends ParseResult[Nothing]

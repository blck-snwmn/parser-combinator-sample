package parser

sealed abstract class ParseResult[+T] {
  def map[U](f: T => U): ParseResult[U] = {
    this match {
      case ParseSuccess(r, n) =>
        ParseSuccess(f(r), n)
      case _ => this.asInstanceOf[ParseResult[U]]
    }
  }

  def flatMap[U](f: (T, String) => ParseResult[U]): ParseResult[U] = {
    this match {
      case ParseSuccess(r, n) => f(r, n)
      case _ => this.asInstanceOf[ParseResult[U]]
    }
  }

  def fold[U](fs: (T, String) => U, ff: String => U): U = {
    this match {
      case ParseSuccess(r, n) => fs(r, n)
      case ParseFailure(m) => ff(m)
    }
  }
}

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult[T]

final case class ParseFailure(val errorMessage: String) extends ParseResult[Nothing]

package parser

sealed abstract class ParseResult[+T] {
  self =>
  def map[U](f: T => U): ParseResult[U] = this match {
    case ParseSuccess(r, n) =>
      ParseSuccess(f(r), n)
    case _ => this.asInstanceOf[ParseResult[U]]
  }

  def flatMap[U](f: (T, String) => ParseResult[U]): ParseResult[U] = this match {
    case ParseSuccess(r, n) => f(r, n)
    case _ => this.asInstanceOf[ParseResult[U]]
  }

  def fold[U](ff: String => U, fs: (T, String) => U): U = this match {
    case ParseSuccess(r, n) => fs(r, n)
    case ParseFailure(m) => ff(m)
  }

  def withFilter(p: ((T, String)) => Boolean): WithFilter = new WithFilter(p)

  class WithFilter(p: ((T, String)) => Boolean) {
    def map[U](f: ((T, String)) => (U, String)): ParseResult[U] = self match {
      case ParseSuccess(r, n) =>
        val t = f((r, n))
        ParseSuccess(t._1, t._2)
      case _ => self.asInstanceOf[ParseResult[U]]
    }

    def flatMap[U](f: ((T, String)) => ParseResult[U]): ParseResult[U] = self match {
      case ParseSuccess(r, n) => f((r, n))
      case _ => self.asInstanceOf[ParseResult[U]]
    }
  }

}

final case class ParseSuccess[T](val result: T, val next: String) extends ParseResult[T]

final case class ParseFailure(val errorMessage: String) extends ParseResult[Nothing]

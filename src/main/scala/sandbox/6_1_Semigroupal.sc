import cats.Semigroupal
import cats.syntax.option._
import cats.syntax.apply._

Semigroupal[Option].product(Some(123), Some("abc"))
Semigroupal[Option].product(None, Some("hi"))

// via apply syntax
(Option(123), Option("abc")).tupled
(Option(123), None).tupled

final case class Cat(name: String, born: Int, color: String)

// this is neat: (F[A], F[B], F[C]) => F[f(A,B,C)]
(Option("Mimi"), Option(2010), Option("white")).mapN(
  Cat.apply
)
// or, for example
(List("Mimi"), List(2010), List("white")).mapN(
  Cat.apply
)

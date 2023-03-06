import cats.Semigroupal
import cats.instances.either._
import cats.syntax.apply._ // for tupled
import cats.syntax.parallel._ // for parTupled

type ErrorOrA[A] = Either[Vector[String], A]
val err1: ErrorOrA[Int] = Left(Vector("err 1"))
val err2: ErrorOrA[Int] = Left(Vector("err 2"))

// short-cuts on the first error:
Semigroupal[ErrorOrA].product(err1, err2)
// the same with tupled
(err1, err2).tupled
// parallel semantics, no short cut on the first
(err1, err2).parTupled

val value1: ErrorOrA[Int] = Right(55)
val value2: ErrorOrA[Int] = Right(108)
// short-cuts on the first error:
Semigroupal[ErrorOrA].product(err1, value2)
// ordering changes the result
Semigroupal[ErrorOrA].product(value1, value2)

// the same with tupled
(value2, value1).tupled
// parallel semantics
(err1, err2).parTupled
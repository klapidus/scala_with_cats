import cats.data.OptionT
import cats.syntax.applicative._ // for pure

type ListOption[A] = OptionT[List, A]

// here List is the outer Monad
// whereas Option is the inner Monad

val result1 = OptionT(List(Option(10)))
val result2 = 32.pure[ListOption]

// the transformer: OptionT, StateT
// always specifies the _inner_ monad

type ErrorOr[A] = Either[String, A]
type ErrorOrOption[A] = OptionT[ErrorOr, A]
val r1 = 10.pure[ErrorOrOption]
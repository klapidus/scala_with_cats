import cats.Monoid
// Options

def parseInt(str: String): Option[Int] =
  scala.util.Try(str.toInt).toOption

def divide(a: Int, b: Int): Option[Int] =
  if (b == 0) None else Some(a/b)

// divide ... => Option[Int]
def stringDivideByFive(aStr: String): Option[Int] =
  parseInt(aStr).flatMap { aNum =>
    divide(aNum, 5)
  }
def stringDivideBy(aStr: String, bStr: String): Option[Int] =
  parseInt(aStr).flatMap { aNum =>
    parseInt(bStr).flatMap { bNum =>
      divide(aNum, bNum)
    }
  }

val myOpt: Option[Int] = None
myOpt.map(toString)
myOpt.flatMap((x: Int) => Some(x.toString))

// Every monad is also a functor
// Or: monad is a functor equipped with flatMap

// using for comprehension
def stringDivideByForC(aStr: String, bStr: String) =
  for {
    aNum <- parseInt(aStr)
    bNum <- parseInt(bStr)
    ans  <- divide(aNum, bNum)
  } yield ans

// Monadic behaviour is captured in two operations:
// pure: A => F[A] (also called unit in literature)
// (further also called point or return)
// flatMap: (F[A], A => F[B]) => F[B] (also called bind, >>=)

trait Monad[F[_]] {
  def pure[A](a: A): F[A]
  def flatMap[A, B](fa: F[A])(func: A => F[B]): F[B]
  def map[A, B](fa: F[A])(func: A => B): F[B] =
    flatMap(fa){
      (x: A) => pure(func(x))
    }
}

// Monads in Cats
import cats.Monad
import cats.instances.option._   // Monad[Option]
import cats.instances.list._     // Monad[List]
import cats.syntax.functor._     // for map
import cats.syntax.flatMap._     // for flatMap
import cats.syntax.applicative._ // for pure

val opt1 = Monad[Option].pure(3)
val opt2 = Monad[Option].flatMap(opt1)(a => Some(a+105))

1.pure[Option]
1.pure[List]

def sumSquare[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] = {
  // inside flat map we have
  // Int => F[fun(Int)]
  a.flatMap(x => b.map(y => x*x + y*y))
}

sumSquare(Option(1), Option(2))
sumSquare(List(1, 2, 3), List(1, 2, 3))

// the same can be written as:
def sumSquareFor[F[_]: Monad](a: F[Int], b: F[Int]): F[Int] =
  for {
    x <- a
    y <- b
  } yield (x*x + y*y)

sumSquareFor(Option(3), Option(4))

// however, we can not do:
//sumSquareFor(3, 4)

// for this, the Identity monad is useful:
// this allows to abstract over monadic and non-monadic code
import cats.Id

//type Id[A] = A
sumSquareFor(3.pure[Id], 4.pure[Id])

// 4.3.1
def pureId[A](v: A): Id[A] = v
def mapId[A, B](fa: Id[A])(f: A => B): Id[B] =
  f(fa)
// next, finish the Id exercise



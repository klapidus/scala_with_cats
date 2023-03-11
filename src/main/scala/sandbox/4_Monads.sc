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

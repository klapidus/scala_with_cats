//Functor laws: identity and composition

// List - type constructor
// List[A] - type

// here F[_] is a higher-kinded type
trait Functor[F[_]] {
  def map[A,B](fa: F[A])(f: A => B): F[B]
}

// F[_] is

import cats.Functor // interface
import cats.instances.list._

Functor[List].map(List(1,2,3))(_ + 107)
// this is obviously the same as:
List(1, 2, 3).map(_ + 107)

val fun1 = (x: Int) => x + 107
val liftedFun1 = Functor[List].lift(fun1)
// here the function is lifted
// A => B lifted to F[A] => F[B]
liftedFun1(List(103, 102))
// this won't work, fun1 is lifted into the List context
// liftedFun1(Option(108))
Functor[List].as(List(100, 200, 300), "bbb")

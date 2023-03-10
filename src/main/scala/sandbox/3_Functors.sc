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

// next proceed with 3.5.2
//

implicit val optionFunctor = new Functor[Option] {
  def map[A, B](fa: Option[A])(f: A => B): Option[B] = {
    // this is a trivial example to illustrate the machinery
    fa map f
  }
}

sealed trait Tree[+A]
final case class Branch[A](l: Tree[A], r: Tree[A])
  extends Tree[A]
final case class Leaf[A](v: A) extends Tree[A]

implicit val functorTree = new Functor[Tree] {
   def map[A, B](fa: Tree[A])(f: A => B): Tree[B] = fa match {
     case Leaf(v) => Leaf(f(v))
     case Branch(l, r) =>
       Branch(map(l)(f), map(r)(f))
   }
}

Functor[Tree].map(Leaf[Int](7))(_ * 2)
Functor[Tree].map(Branch(Leaf(5), Leaf(7)))(_ * 2)

// this won't work, because the way type enrichment works
// it can find only Tree.map, but not Branch.map
// Branch(Leaf(5), Leaf(7)).map(_ * 2)








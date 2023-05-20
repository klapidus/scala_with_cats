def show[A](list: List[A]): String =
  list.foldLeft("nil")((accum, item) => s"$item then $accum")

show(Nil)
show(List(1, 2, 3))

def mapWithFold[A,B](list: List[A])(f: A => B): List[B] =
  list.foldRight(List.empty[B])(
    (item, accum) => f(item) :: accum
  )

def flatMapWithFold[A,B](list: List[A])(f: A => List[B]): List[B] =
  list.foldRight(List.empty[B])(
    (item, accum) => f(item) ::: accum
  )


mapWithFold(List(1, 2, 3))((x: Int) => (x*x + 108).toString())
flatMapWithFold(List(1, 2, 3))((x: Int) => List(x, x+2))


import cats.Foldable
import cats.instances.list._ // for Foldable

val ints = List(1, 2, 3)
Foldable[List].foldLeft(ints, 0)(_ + _)

// foldMap maps over the sequence and combines the results
// using a Monoid
Foldable[List].combineAll(List(1, 2, 3))

Foldable[List].foldMap(List(1, 2, 3))(_.toString)

val ints = List(Vector(1, 2, 3), Vector(4, 5, 6))
(Foldable[List] compose Foldable[Vector]).combineAll(ints)
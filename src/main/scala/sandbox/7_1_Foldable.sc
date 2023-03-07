// folding recap:
def showList[A](list: List[A]): String = {
  //z: B base case Nil => z
  //(B, A) => B
  list.foldLeft("nil")((accum, item) => s"$accum + $item")
}
showList(Nil)
showList(List.empty[Int])
showList(List(1, 7, 12))

//7.1.2 Exercise
List(1,2,3).foldLeft(List.empty[Int])(
  (acc, item) => item :: acc
)

//7.1.3 Exercise
def map[A,B](list: List[A])(f: A => B): List[B] = {
  list.foldLeft(List.empty[B])((acc, item) => f(item) :: acc)
}
def flatMap[A,B](list: List[A])(f: A => List[B]): List[B] = {
  // be au ti ful
  list.foldLeft(List.empty[B])((acc, item) => f(item) ::: acc)
}
flatMap(List(1, 2, 3))(x => List(x+2, x+3))

//foldable in Cats
import cats.Foldable
import cats.instances.list._

val ints = List(105, 108, 111)
Foldable[List].foldLeft(ints, 0)(_ + _)


// compose Foldables for deeper traversal:
import cats.instances.vector._
val nestedInts = List(Vector(1, 2, 3), Vector(107, 106, 105))
// this is a neat example
(Foldable[List] compose Foldable[Vector]).combineAll(nestedInts)

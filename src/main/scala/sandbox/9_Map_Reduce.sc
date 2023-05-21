import scala.concurrent.Future
import scala.concurrent.ExecutionContext.Implicits.global
import cats.Monoid
import cats.syntax.semigroup._ // for |+|

// todo review again B: Monoid construction
// |+| will use an appropriate operation from a Monoid instance
def foldMap[A, B : Monoid](s: Vector[A])(f: A => B): B =
  //s.map(f).foldLeft(Monoid[B].empty)(_ |+| _)
  // this simplification is really neat!
  s.foldLeft(Monoid[B].empty)(_ |+| f(_))

foldMap(Vector(1, 2, 3))(_.toString + " ")

Runtime.getRuntime.availableProcessors
// partition a sequence
//(1 to 10).toList.grouped(3).toList

def parallelFoldMap[A, B : Monoid](s: Vector[A])(f: A => B): Future[B] = {
  // create a list of futures
  val ncpu = Runtime.getRuntime.availableProcessors
  val groupSize = (1.0 * s.size / ncpu).ceil.toInt
  val groups: Iterator[Vector[A]] = s.grouped(groupSize)
  // TODO: finish the implementation

}

import cats.Monad

sealed trait Tree[+A]
final case class Leaf[A](v: A) extends Tree[A]
final case class Branch[A](l: Tree[A], r: Tree[A]) extends Tree[A]

val treeMonad = new Monad[Tree] {
  def pure[A](x: A): Leaf[A] = Leaf(x)
  def flatMap[A, B](fa: Tree[A])
                   (f: A => Tree[B]): Tree[B] = fa match {
    case Leaf(v) => f(v)
    case Branch(l, r) => Branch(flatMap(l)(f), flatMap(r)(f))
  }

  // tailRecM skipped at this iteration!
  def tailRecM[A, B](a: A)
                    (func: A => Tree[Either[A, B]]): Tree[B] =
    flatMap(func(a)) {
      case Left(value) =>
        tailRecM(value)(func)
      case Right(value) =>
        Leaf(value)
    }

}
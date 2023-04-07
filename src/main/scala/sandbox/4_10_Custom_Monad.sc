import cats.Monad
import scala.annotation.tailrec

val optionMonad = new Monad[Option] {
  def pure[A](x: A): Option[A] = Some(x)
  def flatMap[A,B](fa: Option[A])(f: A => Option[B]): Option[B] = fa match {
    case Some(x) => f(x)
    case None => None
  }

  // skipping reading about tailRecM at this iteration!
  @tailrec
  def tailRecM[A, B](a: A)
                    (fn: A => Option[Either[A, B]]): Option[B] =
    fn(a) match {
      case None => None
      case Some(Left(a1)) => tailRecM(a1)(fn)
      case Some(Right(b)) => Some(b)
    }

}

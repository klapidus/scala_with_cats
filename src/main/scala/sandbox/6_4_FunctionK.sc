import cats.arrow.FunctionK
//import cats.~>

object optionToList extends FunctionK[Option, List] {
  def apply[A](fa: Option[A]): List[A] = fa match {
    case None => List.empty[A]
    case Some(v) => List(v)
  }
}

val v1 = Some("hello")
optionToList(v1)
val v2: Option[String] = None
optionToList(v2)

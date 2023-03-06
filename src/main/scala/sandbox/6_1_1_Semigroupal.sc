import cats.Semigroupal
import cats.instances.option._
import cats.syntax.apply._ //for tupled and mapN

Semigroupal[Option].product(Some(123), Some("abc"))

Semigroupal[Option].product(Some(123), None)

Semigroupal.map2(List(3), List(105))(_ + _)
Semigroupal.map2(Option(3), Option(105))(_ + _)
// note Option.empty usage: None as a subtype of Option[Int]
Semigroupal.map2(Option(3), Option.empty[Int])(_ + _)
//Option.empty[Int]

//F[A], F[B] => F[(A,B)]
(Option(123), Option("abc")).tupled

final case class Cat(name: String, born: Int, color: String)

(
  Option("Garfield"), Option(1920), Option("gray")
).mapN(Cat.apply)
// Monoid: binary operation (associative!) & empty element
// Semigroup: Monoid but no requirement of the empty element
// Monoid = Semigroup equipped with Empty

trait Semigroup[A] {
  def combine(x: A, y: A): A
}

trait Monoid[A] extends Semigroup[A] {
  def empty: A
}

implicit val booleanAndMonoid: Monoid[Boolean] = {
  new Monoid[Boolean] {
    def empty: Boolean = true
    def combine(a: Boolean, b: Boolean) = a && b
  }
}

object Monoid {
  def apply[A](implicit monoid: Monoid[A]) =
    monoid
}

Monoid[Boolean].combine(true, false)

// note the usage of def here
// val won't work due to generics A
implicit def setUnionMonoid[A]: Monoid[Set[A]] = {
  new Monoid[Set[A]] {
    override def empty = Set.empty[A]
    override def combine(x: Set[A], y: Set[A]) = x union y
  }
}

// is it true that set intersection doesn't form a monoid?
// what if the empty element contains all possible elements in A

import cats.Monoid  // type class
import cats.instances.string._  // for Monoid
import cats.syntax.semigroup._  // for |+|

Monoid[String].combine("hello", "bye")
"hello" |+| "bye"

// QTree reference:
// https://twitter.github.io/algebird/datatypes/approx/q_tree.html

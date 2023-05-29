final case class GCounter(counters: Map[String, Int]) {
  def increment(machine: String, amount: Int) = {
    val value = counters.getOrElse(machine, 0) + amount
    // no in-place modification, create a new updated GCounter
    GCounter(counters + (machine -> value))
  }

  def merge(that: GCounter): GCounter =
    GCounter(that.counters ++ this.counters.map {
      case (k, v) =>
        k -> (v max that.counters.getOrElse(k, 0))
    })

  def total: Int =
    counters.values.sum
}

import cats.kernel.CommutativeMonoid

trait BoundedSemiLattice[A] extends CommutativeMonoid[A] {
  def combine(a1: A, a2: A): A
  def empty: A
}

object BoundedSemiLattice {
  implicit val intInstance : BoundedSemiLattice[Int] =
    new BoundedSemiLattice[Int] {
      def combine(a1: Int, a2: Int): Int = a1 max a2
      val empty: Int = 0
    }
  implicit def setInstance[A] : BoundedSemiLattice[Set[A]] =
    new BoundedSemiLattice[Set[A]] {
      override def combine(a1: Set[A], a2: Set[A]) =
        a1 union a2
      override def empty: Set[A] = Set.empty[A]
    }
}

import cats.syntax.semigroup._ // for |+|
// here combineAll will use a correct operation from a monoid
import cats.syntax.foldable._  // for combineAll

final case class GGCounter[A](counters: Map[String, A]) {
  def increment(machine: String, amount: A)
               (implicit m: CommutativeMonoid[A]) = {
    val value = counters.getOrElse(machine, m.empty) |+| amount
    // no in-place modification, create a new updated GCounter
    GGCounter(counters + (machine -> value))
  }
  def merge(that: GGCounter[A])
           (implicit bm: BoundedSemiLattice[A]) =
    GGCounter(this.counters |+| that.counters)
  def total(implicit m: CommutativeMonoid[A]): A =
    counters.values.toList.combineAll
}

trait AGCounter[F[_,_], K, V] {
  def increment(f: F[K,V])(k: K, v: V)
               (implicit m: CommutativeMonoid[V]): F[K, V]

  def merge(f1: F[K, V], f2: F[K, V])
           (implicit bm: BoundedSemiLattice[V]): F[K, V]

  def total(f: F[K, V])
           (implicit m: CommutativeMonoid[V]): V

  object AGCounter {
    def apply[F[_, _], K, V](implicit counter: AGCounter[F, K, V]) =
      counter
  }
}

// todo: revisit type classes and this particular syntax
implicit def mapGCounterInstance[K, V]: GCounter[Map, K, V] =
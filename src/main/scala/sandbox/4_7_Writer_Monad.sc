import cats.data.Writer
import cats.instances.vector._
import cats.syntax.applicative._

Writer(Vector("one", "two"), 108108)

type Logged[A] = Writer[Vector[String], A]

// note that Monoid[Vector] must be in scope
// to create an empty "log" part of a Writer instance
123.pure[Logged]

import cats.syntax.writer._ // for tell
("msg1", "msg2", "msg3").tell

// both a log and a result:
Writer(Vector("msg108", "msg109"), 2128506)
// OR
val w = 2128506.writer(Vector("msg1", "msg"))
val w2 = Vector("msg1", "msg").writer(2128506)

w.value
w.written

val (log, result) = w.run
log
result

// the log is preserved while mapping

val writer1 = for {
  a <- 10.pure[Logged]
  _ <- Vector("a", "b", "c").tell
  b <- 32.writer(Vector("x", "y", "z"))
} yield a + b

writer1.run

val writer2 = {
  // the map is needed because the log is a Vector
  writer1.mapWritten(_.map(_.toUpperCase))
}

// transform both log and result:
val writer3 = writer1.bimap(
  log => log.map(_.toUpperCase),
  res => res * 10
)

// reset the log
writer3.reset

// Exercise 4.7.3
// TODO
def slowly[A](body: => A) =
  try body finally Thread.sleep(100)

def factorial(n: Int): Writer[Vector[String], Int] = {
  val ans = slowly(
    if (n==0) 1.writer(Vector("fact 0"))
    else factorial(n - 1).bimap(
      v => v :+ s"fact ${n-1}",
      res => n * res
    )
  )
//  println(s"fact $n $ans")
  ans
}

factorial(5)

import scala.concurrent._
import scala.concurrent.ExecutionContext.Implicits._
import scala.concurrent.duration._
Await.result(Future.sequence(Vector(
  Future(factorial(5)),
  Future(factorial(5))
)), 5.seconds)
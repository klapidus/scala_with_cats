// Evaluation models:
// eager: call-by-value,
// lazy: call-by-name,
// memoization of the result: call-by-need

// call-by-value example
// computation is eager and memoized
val x = {
  println("Computing X")
  math.random
}
x
x

// call-by-name evaluation
// evaluation is lazy and it's not memoized
// (evaluated each time)
def y = {
  println("Computing Y")
  math.random
}
y
y

// lazy and memoized
lazy val z = {
  println("Computing Z")
  math.random
}
z
z

import cats.Eval

val now = Eval.now(math.random + 1000)
val always = Eval.always(math.random + 1000)
val later = Eval.later(math.random + 1000)

now.value     // eager
always.value  // lazy
later.value   // lazy and memoized

val saying = Eval
  .always{ println("Step 1"); "The cat" }
  .map{ str => println("Step 2"); s"$str sat on" }
  .memoize
  .map{ str => println("Step 3"); s"$str the mat"}

// first time call
saying.value
// second time call
saying.value

// Compare with:
val saying = Eval
  .later{ println("Step 1"); "The cat" }
  .map{ str => println("Step 2"); s"$str sat on" }
  .memoize
  .map{ str => println("Step 3"); s"$str the mat"}

saying.value
saying.value

// Trampolining and Eval.defer
def factorial(n: BigInt): BigInt =
  if (n==1) n else n * factorial(n - 1)

// fails with StackOverflowError:
//val f = factorial(50000)
def factorialEval(n: BigInt): Eval[BigInt] =
  if (n==1) {
    Eval.now(n)
  } else {
    Eval.defer(factorialEval(n - 1)).map(_ * n)
  }

// this works now!
factorialEval(50000).value

// TODO: exercise 4.6.5
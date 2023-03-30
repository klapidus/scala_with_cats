import cats.data.State

val a = State[Int, String] { state =>
  (state + 10, s"The state is $state")
}

val (state, result) = a.run(10).value
val stateOnly = a.runS(10).value
val resultOnly = a.runA(10).value

// S => (S, A)

// here s is the initial state
val step1 = State[Int, String] { s =>
  val sNew = s + 1
  (sNew, s"Result of step1: $sNew")
}

step1.run(10).value

val step2 = State[Int, String] { s =>
  val sNew = 2 * s
  (sNew, s"Result of step2: $sNew")
}

val stateComposed = for {
  a <- step1
  b <- step2
} yield (a, b)

stateComposed.run(10).value

// state is threaded from step to step

// get extract the state as the result
val getDemo = State.get[Int]
getDemo.run(10).value

// set sets the state, unit as the result
val setDemo = State.set[Int](30)
setDemo.run(10).value

// pure ignores the state, returns the supplied res
val pureDemo = State.pure[Int, String]("Result")
pureDemo.run(10).value

// inspect: like get, but with transofrm function
val inspectDemo = State.inspect[Int, String](x => s"${x}!")
inspectDemo.run(10).value

// modify: updates the state with a provided function:
val modifyDemo = State.modify[Int](_ + 1)
modifyDemo.run(10).value

//to do: example with for
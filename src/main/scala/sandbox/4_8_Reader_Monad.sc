import cats.data.Reader

final case class Cat(name: String, favoriteFood: String)

// create a Reader[A,B] from function A => B
val catName: Reader[Cat, String] =
  Reader(cat => cat.name)

catName.run(Cat("kiki", "fish"))

// map computation composition (further B => f(B): C)
val greetCat: Reader[Cat, String] =
  catName.map(name => s"hello $name")

val feedCat: Reader[Cat, String] =
  Reader(cat => s"get some ${cat.favoriteFood}")

// reader composition with flatMap
val greetAndFeed: Reader[Cat, String] =
  for {
    greet <- greetCat
    feed  <- feedCat
  } yield s"$greet. $feed"

greetAndFeed.run(Cat("kiki", "fish"))

val hungryCatFromName: Reader[String, Cat] =
  Reader(name => Cat(name, ""))

// Exercise 4.8.3: TODO
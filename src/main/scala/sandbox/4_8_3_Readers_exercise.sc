import cats.data.Reader
import cats.syntax.applicative._ // for pure

final case class Db(usernames: Map[Int, String],
                    passwords: Map[String, String])

type DbReader[A] = Reader[Db, A]

def findUserName(userId: Int): DbReader[Option[String]] =
  Reader(db => db.usernames.get(userId))

def checkPassword(username: String,
                  password: String): DbReader[Boolean] =
  //Reader(db => {
  //  db.passwords.get(username) == password
  //})
  Reader(db => db.passwords.get(username).contains(password))

def checkLogin(userId: Int, password: String): DbReader[Boolean] = {
  for {
    username <- findUserName(userId)
    result <- checkPassword(username.getOrElse(""), password)
  } yield result
}

// also with getOrElse( false.pure[DbReader] )
false.pure[DbReader]

val users = Map(
  1 -> "dade",
  2 -> "kate",
  3 -> "margo"
)

val passwords = Map(
  "dade"  -> "zerocool",
  "kate"  -> "acidburn",
  "margo" -> "secret"
)

val db = Db(users, passwords)
checkLogin(1, "zerocool").run(db)
checkLogin(4, "davinci").run(db)
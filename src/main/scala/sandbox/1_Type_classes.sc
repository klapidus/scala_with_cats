sealed trait Json
final case class JsObject(get: Map[String, Json]) extends Json
final case class JsString(get: String) extends Json
final case class JsNumber(get: Double) extends Json
final case object JsNull extends Json


trait JsonWriter[A] {
  def write(value: A): Json
}

final case class Person(name: String, email: String)

// type class instances are defined here
// for a particular type
object JsonWriterInstances {
  implicit val personWriter: JsonWriter[Person] = {
    new JsonWriter[Person] {
      def write(p: Person): Json = {
        JsObject(Map("name" -> JsString(p.name),
          "email" -> JsString(p.email)))
      }
    }
  }
  implicit val numberWriter: JsonWriter[Double] = {
    new JsonWriter[Double] {
      override def write(value: Double): Json =
        JsNumber(value)
    }
  }
}

//interface objects
object Json {
  def toJson[A](value: A)(implicit w: JsonWriter[A]): Json =
    w.write(value)
}

import JsonWriterInstances._
Json.toJson(Person("kiki", "kiki@kiki.com"))

object JsonSyntax {
  implicit class JsonWriterOps[A](value: A) {
    def toJson(implicit w: JsonWriter[A]): Json =
      w.write(value)
  }
}

import JsonSyntax.JsonWriterOps
// compiler is searching toJson
Person("m", "m@m.m").toJson

// usage with context bound
// this is a shorthand for:
// def myFun[a](v: A)(implicit w: JsonWriter[A]): String
def myFun[A: JsonWriter](v: A): String = {
  val jsonVal = v.toJson
  s"jsonVal"
}

myFun(123.0)

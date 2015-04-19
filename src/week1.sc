abstract class Json
case class Seq(es: List[Json]) extends Json
case class Obj(bindings: Map[String, Json]) extends Json
case class Num(n: Double) extends Json
case class Str(s: String) extends Json
case class Bool(b: Boolean) extends Json
case object Null extends Json

object Json {
  def show(j: Json): String = j match {
    case Num(n) => n.toString
    case Str(s) => '\"' + s + '\"'
    case Bool(b) => b.toString
    case Obj(bindings) =>
      val assocs = bindings map {
        case (k, v) => "\"" + k + "\": " + show(v)
      }
      "{" + (assocs mkString ",") + "}"
    case Seq(es) =>
      val seqs = es map { case e => show(e)}
      "[" + (seqs mkString ", ") + "]"
    case Null => "null"
  }
}

// example json obj.
Json.show(Obj(Map(
  "f" -> Seq(List(Str("a"), Str("b"))),
  "g" -> Seq(List(Str("a"), Str("b")))
)))

// list indexing
val l = List(1, 2, 3)
l(0) // List[E] extends the function Int => E
val m = Map(1 -> "One", 2 -> "Two")
m(1) // Map[K, V] extends the function K => V

/**
 * partial fns
 */
// Here's a simple fn
val f: String => String = {case "ping" => "pong"}
f("ping")
// f("pong") // throws MatchError

// Or we use a partial function
val g: PartialFunction[String, String] = {case "ping" => "pong"}
g("ping")
g.isDefinedAt("pong")
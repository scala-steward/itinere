package itinere
import java.util.UUID

import org.specs2.ScalaCheck
import org.specs2.mutable.Specification
import itinere.circe._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.matcher.Matchers

class CirceSpec extends Specification with ScalaCheck with ArbDerivation with CirceJsonLike with Matchers {

  val encoder: ToJson[SupportedTypes] = jsonEncoder(SupportedTypes.json)
  val decoder: FromJson[SupportedTypes] = jsonDecoder(SupportedTypes.json)

  "must preserve symmetry when encode and decode" >> prop { t: SupportedTypes =>
    decoder.decode(encoder.encode(t)) must beEqualTo(Attempt.success(t))
  }

  /** Parameters to configure scalacheck generators */
  implicit override def parameters: Gen.Parameters = Gen.Parameters.default

  implicit val arbColor: Arbitrary[Color] = Arbitrary(Gen.oneOf(Color.all.toSeq))
}

sealed trait BookingProcess
object BookingProcess {
  case class InProgress(name: String) extends BookingProcess
  case class Done(at: Long) extends BookingProcess
  case class Cancelled(reason: String, at: Long) extends BookingProcess

  val json: Json[BookingProcess] = {

    val inProgress = Json.object1("InProgress")(InProgress.apply)("name" -> member(Json.string, _.name))
    val done = Json.object1("Done")(Done.apply)("name"                   -> member(Json.long, _.at))
    val cancelled = Json.object2("Cancelled")(Cancelled.apply)("reason"  -> member(Json.string, _.reason), "at" -> member(Json.long, _.at))

    (inProgress | done | cancelled).as[BookingProcess]
  }
}

sealed abstract class Color(val name: String)
object Color {
  case object Green extends Color("green")
  case object Red extends Color("red")

  val all = Set(Green, Red)

  def fromString(str: String): Option[Color] = all.find(_.name == str.toLowerCase())

  val json: Json[Color] = Json.string.pmap(x => Attempt.fromOption(fromString(x), s"Cannot find color `$x`"))(_.name)
}

final case class UserId(id: Long) extends AnyVal

object UserId {
  val json: Json[UserId] = Json.long.imap(UserId.apply)(_.id)
}

final case class SupportedTypes(
  int: Int,
  long: Long,
  double: Double,
  float: Float,
  string: String,
  boolean: Boolean,
  uuid: UUID,
  option: Option[Int],
  list: List[Int],
  vector: Vector[Int],
  set: Set[Int],
  seq: Seq[Int],
  either: Either[Int, String],
  color: Color,
  userId: UserId,
  bookingProcess: BookingProcess
)

object SupportedTypes {

  val json: Json[SupportedTypes] = Json.object16("AllTypes")(SupportedTypes.apply)(
    "int"            -> member(Json.int, _.int),
    "long"           -> member(Json.long, _.long),
    "double"         -> member(Json.double, _.double),
    "float"          -> member(Json.float, _.float),
    "string"         -> member(Json.string, _.string),
    "boolean"        -> member(Json.bool, _.boolean),
    "uuid"           -> member(Json.uuid, _.uuid),
    "option"         -> member(Json.option(Json.int), _.option),
    "list"           -> member(Json.list(Json.int), _.list),
    "vector"         -> member(Json.vector(Json.int), _.vector),
    "set"            -> member(Json.set(Json.int), _.set),
    "seq"            -> member(Json.seq(Json.int), _.seq),
    "either"         -> member(Json.or(Json.int, Json.string), _.either),
    "color"          -> member(Color.json, _.color),
    "userId"         -> member(UserId.json, _.userId),
    "bookingProcess" -> member(BookingProcess.json, _.bookingProcess)
  )
}

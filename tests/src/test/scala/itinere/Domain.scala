package itinere
import java.util.UUID

import eu.timepit.refined._
import eu.timepit.refined.string._
import eu.timepit.refined.numeric._
import eu.timepit.refined.api.Refined
import eu.timepit.refined.numeric.Interval
import eu.timepit.refined.types.numeric.{PosInt, PosLong}
import itinere.refined._

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

final case class AuthInfo(
  token: PosInt,
  validTill: PosLong
)

final case class ListFilter(
  ageGreater: Option[PosInt],
  nameStartsWith: Option[String]
)

final case class RegisterUser(
  name: String Refined MatchesRegex[W.`"([A-Za-z]{3,32})"`.T],
  age: Int Refined Interval.Open[W.`0`.T, W.`150`.T]
)

final case class User(
  id: PosLong,
  name: String,
  age: Int Refined Interval.Open[W.`0`.T, W.`150`.T]
)
object User {
  val json: Json[User] = Json.object3("User")(User.apply)(
    "id"   -> member(Json.long.positive, _.id),
    "name" -> member(Json.string, _.name),
    "age"  -> member(Json.int.intervalOpen, _.age)
  )
}

object RegisterUser {
  val json: Json[RegisterUser] = Json.object2("RegisterUser")(RegisterUser.apply)(
    "name" -> member(Json.string.matchesRegex, _.name),
    "age"  -> member(Json.int.intervalOpen, _.age)
  )
}

sealed trait DomainResponse[+A]
object DomainResponse {
  final case class Success[A](value: A) extends DomainResponse[A]
  final case class BadRequest(error: String) extends DomainResponse[Nothing]
  final case class NotFound(error: String) extends DomainResponse[Nothing]

  def success[A](value: Json[A]): Json[Success[A]] = Json.object1("Success")(Success[A](_))("value" -> member(value, _.value))
  val badRequest: Json[BadRequest] = Json.object1("BadRequest")(BadRequest.apply)("error"           -> member(Json.string, _.error))
  val notFound: Json[NotFound] = Json.object1("NotFound")(NotFound.apply)("error"                   -> member(Json.string, _.error))
}

package itinere.openapi

import cats.data.{NonEmptyList, Writer}
import cats.implicits._
import cats.{Applicative, Monoid, Traverse}
import itinere._
import qq.droste._
import qq.droste.data._
import qq.droste.util.DefaultTraverse
import shapeless.CNil

final case class OpenApiServer(url: String, description: Option[String] = None)

sealed trait OpenApiType

object OpenApiType {
  case object String extends OpenApiType
  case object Number extends OpenApiType
  case object Integer extends OpenApiType
  case object Boolean extends OpenApiType
}
sealed trait OpenApiFormat

object OpenApiFormat {
  case object Int32 extends OpenApiFormat
  case object Int64 extends OpenApiFormat
  case object Float extends OpenApiFormat
  case object Double extends OpenApiFormat
  case object Byte extends OpenApiFormat
  case object Binary extends OpenApiFormat
  case object Boolean extends OpenApiFormat
  case object Date extends OpenApiFormat
  case object DateTime extends OpenApiFormat
  case object Password extends OpenApiFormat
}

final case class OpenApiParameter(
  name: String,
  in: OpenApiParameter.In,
  required: Boolean,
  description: Option[String],
  `type`: OpenApiType = OpenApiType.String,
  format: Option[OpenApiFormat] = None //TODO: consider this enumeration
)

object OpenApiParameter {
  sealed trait In

  object In {
    case object Path extends In
    case object Query extends In
    case object Header extends In
    case object Form extends In
  }

  def query(name: String, description: Option[String] = None, `type`: OpenApiType = OpenApiType.String, format: Option[OpenApiFormat] = None) =
    OpenApiParameter(
      name = name,
      in = In.Query,
      required = false,
      description = description,
      `type` = `type`,
      format = format
    )

  def path(name: String, description: Option[String] = None, `type`: OpenApiType = OpenApiType.String, format: Option[OpenApiFormat] = None) =
    OpenApiParameter(
      name = name,
      in = In.Path,
      required = true,
      description = description,
      `type` = `type`,
      format = format
    )
}

sealed trait OpenApiBodyEntity

object OpenApiBodyEntity {
  case class Json(schema: JsonSchema.Type) extends OpenApiBodyEntity
  case object File extends OpenApiBodyEntity
  case object Empty extends OpenApiBodyEntity
}

final case class OpenApiHeader(`type`: OpenApiType, format: Option[OpenApiFormat] = None, description: Option[String] = None)

final case class OpenApiOperation(
  method: String,
  path: OpenApiPath,
  requestBody: OpenApiBodyEntity,
  responses: OpenApiResponses = OpenApiResponses(),
  parameters: List[OpenApiParameter] = Nil,
  summary: Option[String] = None,
  tag: Option[String] = None
) {
  def toReferenceTree: Writer[JsonSchemaManifest, OpenApiOperation] = {
    def cleanupRequestBody: Writer[JsonSchemaManifest, OpenApiBodyEntity] =
      requestBody match {
        case OpenApiBodyEntity.Json(schema) => JsonSchema.cleanup(schema).map(OpenApiBodyEntity.Json)
        case OpenApiBodyEntity.File         => Writer.value(OpenApiBodyEntity.File)
        case OpenApiBodyEntity.Empty        => Writer.value(OpenApiBodyEntity.Empty)
      }

    def cleanupResponses: Writer[JsonSchemaManifest, OpenApiResponses] =
      responses.byStatusCode.toList
        .traverse[Writer[JsonSchemaManifest, ?], (Int, OpenApiResponse)] {
          case (statusCode, resp) =>
            resp.responseBody match {
              case OpenApiBodyEntity.Json(schema) => JsonSchema.cleanup(schema).map(ss => statusCode -> OpenApiResponse(resp.description, resp.headers, OpenApiBodyEntity.Json(ss), resp.example))
              case OpenApiBodyEntity.File         => Writer.value(statusCode -> resp)
              case OpenApiBodyEntity.Empty        => Writer.value(statusCode -> resp)
            }
        } map (x => OpenApiResponses(x.toMap))

    for {
      b <- cleanupRequestBody
      r <- cleanupResponses
    } yield
      copy(
        responses = r,
        requestBody = b
      )
  }
}

case class OpenApiInfo(title: String, version: String, description: String)
case class OpenApiRoot(info: OpenApiInfo, operations: List[OpenApiOperation], definitions: JsonSchemaManifest, servers: Set[OpenApiServer])
object OpenApiRoot {
  def toJson(root: OpenApiRoot): Json.Type = {

    def schemaManifest(a: JsonSchemaManifest): Json.Type =
      Json.obj(a.manifest.toList.map { case (id, schema) => id -> JsonSchema.toJson(schema) }: _*)

    def openApiType(a: OpenApiType): Json.Type = a match {
      case OpenApiType.String  => Json.fromString("string")
      case OpenApiType.Number  => Json.fromString("number")
      case OpenApiType.Integer => Json.fromString("integer")
      case OpenApiType.Boolean => Json.fromString("boolean")
    }

    def openApiFormat(a: OpenApiFormat): Json.Type = a match {
      case OpenApiFormat.Int32    => Json.fromString("int32")
      case OpenApiFormat.Int64    => Json.fromString("int64")
      case OpenApiFormat.Float    => Json.fromString("float")
      case OpenApiFormat.Double   => Json.fromString("double")
      case OpenApiFormat.Byte     => Json.fromString("byte")
      case OpenApiFormat.Binary   => Json.fromString("binary")
      case OpenApiFormat.Boolean  => Json.fromString("boolean")
      case OpenApiFormat.Date     => Json.fromString("date")
      case OpenApiFormat.DateTime => Json.fromString("datetime")
      case OpenApiFormat.Password => Json.fromString("password")
    }

    def openApiParameterIn(a: OpenApiParameter.In): Json.Type = a match {
      case OpenApiParameter.In.Path   => Json.fromString("path")
      case OpenApiParameter.In.Query  => Json.fromString("query")
      case OpenApiParameter.In.Header => Json.fromString("header")
      case OpenApiParameter.In.Form   => Json.fromString("form")
    }

    def openApiParameter(a: OpenApiParameter): Json.Type =
      Json.obj(
        "name"        -> Json.fromString(a.name),
        "in"          -> openApiParameterIn(a.in),
        "description" -> a.description.fold(Json.nil)(Json.fromString),
        "required"    -> Json.fromBoolean(a.required),
        "schema" -> Json.obj(
          "type"   -> openApiType(a.`type`),
          "format" -> a.format.fold(Json.nil)(openApiFormat)
        )
      )

    def openApiBodyEntity(a: OpenApiBodyEntity): Json.Type = a match {
      case OpenApiBodyEntity.Json(schema) =>
        Json.obj("application/json" -> Json.obj("schema" -> JsonSchema.toJson((schema))))
      case OpenApiBodyEntity.File =>
        Json.obj("application/octet-stream" -> Json.obj("schema" -> Json.obj("type" -> Json.fromString("string"), "format" -> Json.fromString("binary"))))
      case OpenApiBodyEntity.Empty => Json.nil
    }

    def openApiResponse(a: OpenApiResponse): Json.Type = Json.obj(
      "content"     -> openApiBodyEntity(a.responseBody),
      "description" -> Json.fromString(a.description),
      "example"     -> a.example.fold(Json.nil)(Json.fromString)
    )

    def foldObject(in: Json.Type)(other: => Json.Type, ifObject: Json.Type => Json.Type): Json.Type = Fix.un(in) match {
      case u @ JsonF.Object(_) => ifObject(Fix(u))
      case _                   => other
    }

    def openApiOperation(a: OpenApiOperation): Json.Type = Json.obj(
      "tags"        -> Json.arr(a.tag.toList.map(Json.fromString)),
      "parameters"  -> Json.arr((a.path.parameters ++ a.parameters).map(openApiParameter)),
      "summary"     -> a.summary.fold(Json.nil)(Json.fromString),
      "requestBody" -> foldObject(openApiBodyEntity(a.requestBody))(Json.nil, obj => Json.obj("content" -> obj)),
      "responses" ->
      Json.obj(a.responses.byStatusCode.toList.map { case (code, response) => code.toString -> openApiResponse(response) }: _*)
    )

    def openApiInfo(a: OpenApiInfo): Json.Type = Json.obj(
      "title"       -> Json.fromString(a.title),
      "version"     -> Json.fromString(a.version),
      "description" -> Json.fromString(a.description),
    )

    Json.obj(
      "openapi"    -> Json.fromString("3.0.0"),
      "servers"    -> Json.arr(root.servers.map(s => Json.obj("url" -> Json.fromString(s.url))).toList),
      "info"       -> openApiInfo(root.info),
      "components" -> Json.obj("schemas" -> schemaManifest(root.definitions)),
      "paths" -> Json.fromFields(
        root.operations
          .groupBy(_.path.id)
          .toList
          .map {
            case (path, operations) =>
              path -> Json.obj(operations.map(op => op.method -> openApiOperation(op)): _*)
          }
          .toMap
      )
    )
  }
}

case class JsonSchemaManifest(manifest: Map[String, JsonSchema.Type])

object JsonSchemaManifest {
  val empty = JsonSchemaManifest(Map.empty[String, JsonSchema.Type])

  def single(value: (String, JsonSchema.Type)): JsonSchemaManifest = JsonSchemaManifest(Map(value))

  implicit val monoid: Monoid[JsonSchemaManifest] = new Monoid[JsonSchemaManifest] {
    override def empty: JsonSchemaManifest = JsonSchemaManifest.empty
    override def combine(x: JsonSchemaManifest, y: JsonSchemaManifest): JsonSchemaManifest = JsonSchemaManifest(x.manifest ++ y.manifest)
  }
}

trait ToJsonSchema[A] {
  def schema: JsonSchema.Type
}
object ToJsonSchema {
  def create[A](jsonSchema: JsonSchema.Type): ToJsonSchema[A] = new ToJsonSchema[A] { override def schema: JsonSchema.Type = jsonSchema }

  implicit val algebra: JsonAlgebra[ToJsonSchema] = new JsonAlgebra[ToJsonSchema] with ToJsonSchemaFormatN {
    override def int(bound: itinere.Range): ToJsonSchema[Int] = create(JsonSchema.integer(bound, IntegerType.Int32))
    override def float(bound: itinere.Range): ToJsonSchema[Float] = create(JsonSchema.number(bound, NumberType.Float))
    override def double(bound: itinere.Range): ToJsonSchema[Double] = create(JsonSchema.number(bound, NumberType.Double))
    override def long(bound: itinere.Range): ToJsonSchema[Long] = create(JsonSchema.integer(bound, IntegerType.Int64))
    override def string(description: StringDescriptor): ToJsonSchema[String] = create(JsonSchema.str(description))
    override val bool: ToJsonSchema[Boolean] = create(JsonSchema.bool)
    override val cnil: ToJsonSchema[CNil] = create(JsonSchema.nil)
    override def option[A](from: ToJsonSchema[A]): ToJsonSchema[Option[A]] = create(JsonSchema.optional(from.schema))
    override def list[A](of: ToJsonSchema[A]): ToJsonSchema[List[A]] = create(JsonSchema.array(LengthBound.Unbounded, false, of.schema))
    override def set[A](of: ToJsonSchema[A]): ToJsonSchema[Set[A]] = create(JsonSchema.array(LengthBound.Unbounded, true, of.schema))
    override def vector[A](of: ToJsonSchema[A]): ToJsonSchema[Vector[A]] = create(JsonSchema.array(LengthBound.Unbounded, false, of.schema))
    override def seq[A](of: ToJsonSchema[A]): ToJsonSchema[Seq[A]] = create(JsonSchema.array(LengthBound.Unbounded, false, of.schema))
    override def pmap[A, B](fa: ToJsonSchema[A])(f: A => Attempt[B])(g: B => A): ToJsonSchema[B] = new ToJsonSchema[B] { override def schema: JsonSchema.Type = fa.schema }
    override def sum[A, B](fa: ToJsonSchema[A], fb: ToJsonSchema[B]): ToJsonSchema[Either[A, B]] = create(JsonSchema.anyOf(NonEmptyList.of(fa.schema, fb.schema)))
    override def imap[A, B](fa: ToJsonSchema[A])(f: A => B)(g: B => A): ToJsonSchema[B] = new ToJsonSchema[B] { override def schema: JsonSchema.Type = fa.schema }
    override def nel[A](of: ToJsonSchema[A]): ToJsonSchema[NonEmptyList[A]] = create(JsonSchema.array(LengthBound.Atleast(1), false, of.schema))
    override def enum[A](all: Set[A], show: A => String): ToJsonSchema[A] = create(JsonSchema.enum(all.map(show)))
  }
}

trait TraverseMap {
  protected implicit def mapTraverse[K]: Traverse[Map[K, ?]] = new DefaultTraverse[Map[K, ?]] {
    override def traverse[G[_], A, B](fa: Map[K, A])(f: A => G[B])(implicit G: Applicative[G]): G[Map[K, B]] =
      fa.toList.traverse { case (key, value) => f(value).map(key -> _) }.map(_.toMap)
  }
}

sealed trait JsonF[+A]

object JsonF {
  final case object Null extends JsonF[Nothing]
  final case class Object[A](fields: Map[String, A]) extends JsonF[A]
  final case class Str(value: String) extends JsonF[Nothing]
  final case class Array[A](values: List[A]) extends JsonF[A]
  final case class Bool(value: Boolean) extends JsonF[Nothing]
  final case class Number(number: BigDecimal) extends JsonF[Nothing]

  implicit val traverse: Traverse[JsonF] = new DefaultTraverse[JsonF] with TraverseMap {
    override def traverse[G[_], A, B](fa: JsonF[A])(f: A => G[B])(implicit G: Applicative[G]): G[JsonF[B]] = fa match {
      case JsonF.Null           => G.pure(JsonF.Null)
      case JsonF.Object(fields) => Traverse[Map[String, ?]].traverse(fields)(f).map(JsonF.Object.apply)
      case JsonF.Str(value)     => G.pure(JsonF.Str(value))
      case JsonF.Array(values)  => values.traverse(f).map(values => JsonF.Array(values))
      case JsonF.Bool(value)    => G.pure(JsonF.Bool(value))
      case JsonF.Number(value)  => G.pure(JsonF.Number(value))
    }
  }
}

object Json {
  type Type = Fix[JsonF]

  def nil: Type = Fix[JsonF](JsonF.Null)
  def obj(fields: (String, Type)*): Type = Fix[JsonF](JsonF.Object(fields.toMap))
  def fromString(value: String): Type = Fix[JsonF](JsonF.Str(value))
  def arr(values: List[Type]): Type = Fix[JsonF](JsonF.Array(values))
  def fromBoolean(value: Boolean): Type = Fix[JsonF](JsonF.Bool(value))
  def fromInt(value: Int): Type = Fix[JsonF](JsonF.Number(BigDecimal(value)))
  def fromBigDecimal(value: BigDecimal): Type = Fix[JsonF](JsonF.Number(value))
  def fromFields(fields: Map[String, Type]): Type = Fix[JsonF](JsonF.Object(fields))
}

sealed trait JsonSchemaF[A]
object JsonSchemaF {
  final case class AnyOf[A](choices: NonEmptyList[A]) extends JsonSchemaF[A]
  final case class Ref[A](id: String) extends JsonSchemaF[A]
  final case class Object[A](name: String, fields: Map[String, A], required: Set[String]) extends JsonSchemaF[A]
  final case class Array[A](lengthBound: LengthBound, uniqueItems: Boolean, schema: A) extends JsonSchemaF[A]
  final case class Optional[A](schema: A) extends JsonSchemaF[A]
  final case class Str[A](stringDescriptor: StringDescriptor) extends JsonSchemaF[A]
  final case class Number[A](range: itinere.Range, numberType: NumberType) extends JsonSchemaF[A]
  final case class Integer[A](range: itinere.Range, integerType: IntegerType) extends JsonSchemaF[A]
  final case class Bool[A]() extends JsonSchemaF[A]
  final case class Enum[A](options: Set[String]) extends JsonSchemaF[A]
  final case class Nil[A]() extends JsonSchemaF[A]

  implicit val instance: Traverse[JsonSchemaF] = new DefaultTraverse[JsonSchemaF] with TraverseMap {
    override def traverse[G[_], A, B](fa: JsonSchemaF[A])(f: A => G[B])(implicit G: Applicative[G]): G[JsonSchemaF[B]] = fa match {
      case JsonSchemaF.AnyOf(choices)                          => choices.traverse(f).map(AnyOf.apply)
      case JsonSchemaF.Ref(id)                                 => G.pure(JsonSchemaF.Ref(id))
      case JsonSchemaF.Object(name, fields, required)          => Traverse[Map[String, ?]].traverse(fields)(f).map(newFields => JsonSchemaF.Object(name, newFields, required))
      case JsonSchemaF.Array(lengthBound, uniqueItems, schema) => f(schema).map(b => JsonSchemaF.Array(lengthBound, uniqueItems, b))
      case JsonSchemaF.Optional(schema)                        => f(schema).map(b => JsonSchemaF.Optional(b))
      case JsonSchemaF.Str(stringDescriptor)                   => G.pure(JsonSchemaF.Str(stringDescriptor))
      case JsonSchemaF.Number(range, numberType)               => G.pure(JsonSchemaF.Number(range, numberType))
      case JsonSchemaF.Integer(range, integerType)             => G.pure(JsonSchemaF.Integer(range, integerType))
      case JsonSchemaF.Enum(options)                           => G.pure(JsonSchemaF.Enum(options))
      case JsonSchemaF.Bool()                                  => G.pure(JsonSchemaF.Bool())
      case JsonSchemaF.Nil()                                   => G.pure(JsonSchemaF.Nil())
    }
  }

}

object JsonSchema {
  type Type = Fix[JsonSchemaF]

  def anyOf(choices: NonEmptyList[Type]): Type =
    Fix[JsonSchemaF](JsonSchemaF.AnyOf(choices))
  def enum(opts: Set[String]): Type =
    Fix[JsonSchemaF](JsonSchemaF.Enum(opts))
  def ref(id: String): Type =
    Fix[JsonSchemaF](JsonSchemaF.Ref(id))
  def obj(name: String, fields: Map[String, Type]): Type =
    Fix[JsonSchemaF](JsonSchemaF.Object(name, fields, Set.empty))
  def array(lengthBound: LengthBound, uniqueItems: Boolean, schema: Type): Type =
    Fix[JsonSchemaF](JsonSchemaF.Array(lengthBound, uniqueItems, schema))
  def optional(schema: Type): Type =
    Fix[JsonSchemaF](JsonSchemaF.Optional(schema))
  def str(stringDescriptor: StringDescriptor): Type =
    Fix[JsonSchemaF](JsonSchemaF.Str(stringDescriptor))
  def number(range: itinere.Range, numberType: NumberType): Type =
    Fix[JsonSchemaF](JsonSchemaF.Number(range, numberType))
  def integer(range: itinere.Range, integerType: IntegerType): Type =
    Fix[JsonSchemaF](JsonSchemaF.Integer(range, integerType))
  val bool: Type =
    Fix[JsonSchemaF](JsonSchemaF.Bool())
  val nil: Type =
    Fix[JsonSchemaF](JsonSchemaF.Nil())

  val requiredFields: Trans[JsonSchemaF, JsonSchemaF, Type] = Trans {
    case JsonSchemaF.Object(name, fields, _) =>
      val required = fields.toList.flatMap {
        case (key, value) =>
          Fix.un(value) match {
            case JsonSchemaF.Optional(_) => None
            case _                       => Some(key)
          }
      }

      JsonSchemaF.Object(name, fields, required.toSet)

    case other => other
  }

  val flattenNestedAnyOf: Trans[JsonSchemaF, JsonSchemaF, Type] = Trans {
    case JsonSchemaF.AnyOf(NonEmptyList(head, tail :: Nil)) =>
      Fix.un(tail) match {
        case JsonSchemaF.AnyOf(choices) =>
          JsonSchemaF.AnyOf(head :: choices)
        case JsonSchemaF.Nil() =>
          JsonSchemaF.AnyOf(NonEmptyList.of(head))
        case other =>
          JsonSchemaF.AnyOf(NonEmptyList.of(head, Fix(other)))
      }

    case other => other
  }

  val toManifest: AlgebraM[Writer[JsonSchemaManifest, ?], JsonSchemaF, Type] = AlgebraM {
    case u @ JsonSchemaF.Object(name, _, _) =>
      Writer[JsonSchemaManifest, Type](JsonSchemaManifest.single(name -> Fix(u)), ref(name))
    case s =>
      Writer[JsonSchemaManifest, Type](JsonSchemaManifest.empty, Fix(s))
  }

  def cleanup: Fix[JsonSchemaF] => Writer[JsonSchemaManifest, Type] =
    scheme.cata(requiredFields.algebra) andThen
    scheme.cata(flattenNestedAnyOf.algebra) andThen
    scheme.cataM(toManifest)

  def toJson(jsonSchema: JsonSchema.Type): Json.Type = {

    def lengthBoundToJson(lowField: String, highField: String, bound: LengthBound) = bound match {
      case LengthBound.Exact(value) =>
        List(lowField -> Json.fromInt(value), highField -> Json.fromInt(value))
      case LengthBound.Atleast(low) =>
        List(lowField -> Json.fromInt(low))
      case LengthBound.Atmost(high) =>
        List(highField -> Json.fromInt(high))
      case LengthBound.Interval(low, high) =>
        List(lowField -> Json.fromInt(low), highField -> Json.fromInt(high))
      case LengthBound.Unbounded =>
        Nil
    }

    def rangeToJson(range: Range) =
      List(
        "minimum"          -> Json.fromBigDecimal(range.lower.value),
        "exclusiveMinimum" -> Json.fromBoolean(!range.lower.inclusive),
        "maximum"          -> Json.fromBigDecimal(range.upper.value),
        "exclusiveMaximum" -> Json.fromBoolean(!range.upper.inclusive)
      )

    def algebra: Algebra[JsonSchemaF, Json.Type] = Algebra {
      case JsonSchemaF.Array(lengthBound, uniqueItems, schema) =>
        Json.obj(
          List("type" -> Json.fromString("array"), "uniqueItems" -> Json.fromBoolean(uniqueItems), "items" -> schema) ++ lengthBoundToJson("minItems", "maxItems", lengthBound): _*
        )
      case JsonSchemaF.Enum(options) =>
        Json.obj("type" -> Json.fromString("string"), "enum" -> Json.arr(options.map(Json.fromString).toList))
      case JsonSchemaF.Ref(id) =>
        Json.obj("$ref" -> Json.fromString(s"#/components/schemas/$id"))
      case JsonSchemaF.Bool() =>
        Json.obj("type" -> Json.fromString("boolean"))
      case JsonSchemaF.Integer(range, t) =>
        Json.obj(
          List("type" -> Json.fromString("integer"), "format" -> Json.fromString(t.format)) ++ rangeToJson(range): _*
        )
      case JsonSchemaF.Number(range, t) =>
        Json.obj(
          List("type" -> Json.fromString("number"), "format" -> Json.fromString(t.format)) ++ rangeToJson(range): _*
        )
      case JsonSchemaF.Str(StringDescriptor.Pattern(pattern)) =>
        Json.obj(
          "type"    -> Json.fromString("string"),
          "pattern" -> Json.fromString(pattern)
        )
      case JsonSchemaF.Str(StringDescriptor.Length(lengthBound)) =>
        Json.obj(List("type" -> Json.fromString("string")) ++ lengthBoundToJson("minLength", "maxLength", lengthBound): _*)
      case JsonSchemaF.Str(StringDescriptor.Type(stringType)) =>
        Json.obj("type" -> Json.fromString("string"), "format" -> Json.fromString(stringType.format))
      case JsonSchemaF.Str(StringDescriptor.Unbounded) =>
        Json.obj("type" -> Json.fromString("string"))
      case JsonSchemaF.AnyOf(choices) =>
        Json.obj("anyOf" -> Json.arr(choices.toList))
      case JsonSchemaF.Optional(schema) =>
        schema
      case JsonSchemaF.Object(_, fields, required) =>
        Json.obj(
          "type"       -> Json.fromString("object"),
          "properties" -> Json.obj(fields.map { case (key, value) => key -> value }.toList: _*),
          "required"   -> Json.arr(required.map(Json.fromString).toList)
        )
      case _ => Json.nil
    }

    scheme.cata(algebra).apply(jsonSchema)
  }
}

case class OpenApiResponse(description: String, headers: Map[String, OpenApiHeader] = Map.empty, responseBody: OpenApiBodyEntity, example: Option[String] = None) {
  def withHeaders(headers: Map[String, OpenApiHeader]): OpenApiResponse = copy(headers = headers)
}

case class OpenApiResponses(byStatusCode: Map[Int, OpenApiResponse] = Map.empty)

case class OpenApiPath(id: String, parameters: List[OpenApiParameter] = Nil)

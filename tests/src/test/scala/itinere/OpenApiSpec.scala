package itinere

import io.circe.Printer
import itinere.openapi.{JsonF, JsonSchema, OpenApiGen, OpenApiGenJson, OpenApiInfo, OpenApiRoot, OpenApiServer, ToJsonSchema, Json => J}
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification
import qq.droste._
import io.swagger.v3.parser.OpenAPIV3Parser
import itinere.openapi.circe._

class OpenApiSpec extends Specification with Matchers {

  "OpenApiGen" >> {

    val api = OpenApiEndpoints
    val apiServers = Set(OpenApiServer("http://localhost:8080"))
    val apiInfo = OpenApiInfo("Test API", "1.0", "This is a test api")
    val spec = OpenApiGen.api(apiInfo, apiServers)(api.userDelete, api.userGet, api.userList, api.userRegister)

    "basic test" >> {

      spec.servers must beEqualTo(apiServers)
      spec.info.description must beEqualTo("This is a test api")
      spec.info.title must beEqualTo("Test API")
      spec.info.version must beEqualTo("1.0")
      spec.definitions.manifest.values must have size 5
      spec.operations must have size 4
    }

    "complete endpoint definition should render to json" >> {

      val json = OpenApiRoot.toJson(spec)
      val expected = J.obj(
        "paths" -> J.obj(
          "/users/{userId}/{color}" -> J.obj(
            "delete" -> J.obj(
              "tags"    -> J.arr(List(J.fromString("Users"))),
              "summary" -> J.fromString("Endpoint for deleting users"),
              "responses" -> J.obj(
                "200" -> J.obj(
                  "content"     -> J.obj("application/json" -> J.obj("schema" -> J.obj("type" -> J.fromString("string")))),
                  "description" -> J.fromString("Response in case of deletion"),
                  "example"     -> J.nil
                )
              ),
              "requestBody" -> J.nil,
              "parameters" -> J.arr(
                List(
                  J.obj(
                    "in"          -> J.fromString("path"),
                    "required"    -> J.fromBoolean(true),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("integer"), "format" -> J.nil),
                    "name"        -> J.fromString("userId")
                  ),
                  J.obj(
                    "in"          -> J.fromString("path"),
                    "required"    -> J.fromBoolean(true),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("string"), "format" -> J.nil),
                    "name"        -> J.fromString("color")
                  )
                )
              )
            )
          ),
          "/users" -> J.obj(
            "get" -> J.obj(
              "tags"    -> J.arr(List(J.fromString("Users"))),
              "summary" -> J.fromString("Endpoint for listing users"),
              "responses" -> J.obj(
                "200" -> J.obj(
                  "content"     -> J.obj("application/json" -> J.obj("schema" -> J.obj("type" -> J.fromString("array"), "uniqueItems" -> J.fromBoolean(false), "items" -> J.obj("$ref" -> J.fromString("#/components/schemas/User"))))),
                  "description" -> J.fromString("Response in case of success"),
                  "example"     -> J.nil
                )
              ),
              "requestBody" -> J.nil,
              "parameters" -> J.arr(
                List(
                  J.obj(
                    "in"          -> J.fromString("query"),
                    "required"    -> J.fromBoolean(false),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("integer"), "format" -> J.nil),
                    "name"        -> J.fromString("ageGreater")
                  ),
                  J.obj(
                    "in"          -> J.fromString("query"),
                    "required"    -> J.fromBoolean(false),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("string"), "format" -> J.nil),
                    "name"        -> J.fromString("nameStartsWith")
                  )
                )
              )
            )
          ),
          "/users/register" -> J.obj(
            "post" -> J.obj(
              "tags"    -> J.arr(List(J.fromString("Users"))),
              "summary" -> J.fromString("Endpoint for registration of users"),
              "responses" -> J.obj(
                "200" -> J.obj(
                  "content"     -> J.obj("application/json" -> J.obj("schema" -> J.obj("type" -> J.fromString("string")))),
                  "description" -> J.fromString("Response in case of success"),
                  "example"     -> J.nil
                )
              ),
              "requestBody" -> J.obj("content" -> J.obj("application/json" -> J.obj("schema" -> J.obj("$ref" -> J.fromString("#/components/schemas/RegisterUser"))))),
              "parameters"  -> J.arr(List())
            )
          ),
          "/users/{userId}" -> J.obj(
            "get" -> J.obj(
              "tags"    -> J.arr(List(J.fromString("Users"))),
              "summary" -> J.fromString("Endpoint for getting a specific users"),
              "responses" -> J.obj(
                "400" -> J.obj(
                  "content"     -> J.obj("application/json" -> J.obj("schema" -> J.obj("$ref" -> J.fromString("#/components/schemas/BadRequest")))),
                  "description" -> J.fromString("Response in case of the request body or parameters were in correct"),
                  "example"     -> J.nil
                ),
                "404" -> J.obj(
                  "content"     -> J.obj("application/json" -> J.obj("schema" -> J.obj("$ref" -> J.fromString("#/components/schemas/NotFound")))),
                  "description" -> J.fromString("Response in case of the entity was not found"),
                  "example"     -> J.nil
                ),
                "200" -> J.obj(
                  "content"     -> J.obj("application/json" -> J.obj("schema" -> J.obj("$ref" -> J.fromString("#/components/schemas/Success")))),
                  "description" -> J.fromString("Response in case of success"),
                  "example"     -> J.nil
                )
              ),
              "requestBody" -> J.nil,
              "parameters" -> J.arr(
                List(
                  J.obj(
                    "in"          -> J.fromString("path"),
                    "required"    -> J.fromBoolean(true),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("integer"), "format" -> J.nil),
                    "name"        -> J.fromString("userId")
                  ),
                  J.obj(
                    "in"          -> J.fromString("header"),
                    "required"    -> J.fromBoolean(true),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("integer"), "format" -> J.nil),
                    "name"        -> J.fromString("X-ValidTill")
                  ),
                  J.obj(
                    "in"          -> J.fromString("header"),
                    "required"    -> J.fromBoolean(true),
                    "description" -> J.nil,
                    "schema"      -> J.obj("type" -> J.fromString("integer"), "format" -> J.nil),
                    "name"        -> J.fromString("X-Token")
                  )
                )
              )
            )
          )
        ),
        "info"    -> J.obj("title" -> J.fromString("Test API"), "version" -> J.fromString("1.0"), "description" -> J.fromString("This is a test api")),
        "servers" -> J.arr(List(J.obj("url" -> J.fromString("http://localhost:8080")))),
        "openapi" -> J.fromString("3.0.0"),
        "components" -> J.obj(
          "schemas" -> J.obj(
            "RegisterUser" -> J.obj(
              "type" -> J.fromString("object"),
              "properties" -> J.obj(
                "name" -> J.obj("type" -> J.fromString("string"), "pattern" -> J.fromString("([A-Za-z]{3,32})")),
                "age" -> J.obj(
                  "exclusiveMaximum" -> J.fromBoolean(true),
                  "type"             -> J.fromString("integer"),
                  "maximum"          -> J.fromBigDecimal(BigDecimal("150.0")),
                  "exclusiveMinimum" -> J.fromBoolean(true),
                  "format"           -> J.fromString("int32"),
                  "minimum"          -> J.fromBigDecimal(BigDecimal("0.0"))
                )
              ),
              "required" -> J.arr(List(J.fromString("name"), J.fromString("age")))
            ),
            "Success" -> J.obj(
              "type"       -> J.fromString("object"),
              "properties" -> J.obj("value" -> J.obj("$ref" -> J.fromString("#/components/schemas/User"))),
              "required"   -> J.arr(List(J.fromString("value")))
            ),
            "NotFound" -> J.obj("type" -> J.fromString("object"), "properties" -> J.obj("error" -> J.obj("type" -> J.fromString("string"))), "required" -> J.arr(List(J.fromString("error")))),
            "User" -> J.obj(
              "type" -> J.fromString("object"),
              "properties" -> J.obj(
                "id" -> J.obj(
                  "exclusiveMaximum" -> J.fromBoolean(false),
                  "type"             -> J.fromString("integer"),
                  "maximum"          -> J.fromBigDecimal(BigDecimal("9223372036854775807")),
                  "exclusiveMinimum" -> J.fromBoolean(false),
                  "format"           -> J.fromString("int64"),
                  "minimum"          -> J.fromBigDecimal(BigDecimal("1"))
                ),
                "name" -> J.obj("type" -> J.fromString("string")),
                "age" -> J.obj(
                  "exclusiveMaximum" -> J.fromBoolean(true),
                  "type"             -> J.fromString("integer"),
                  "maximum"          -> J.fromBigDecimal(BigDecimal("150.0")),
                  "exclusiveMinimum" -> J.fromBoolean(true),
                  "format"           -> J.fromString("int32"),
                  "minimum"          -> J.fromBigDecimal(BigDecimal("0.0"))
                )
              ),
              "required" -> J.arr(List(J.fromString("id"), J.fromString("name"), J.fromString("age")))
            ),
            "BadRequest" -> J.obj("type" -> J.fromString("object"), "properties" -> J.obj("error" -> J.obj("type" -> J.fromString("string"))), "required" -> J.arr(List(J.fromString("error"))))
          )
        )
      )

      json must beEqualTo(expected)
    }

    "entity definition should render to json" >> {

      val transformation = scheme.cata(JsonSchema.flattenNestedAnyOf.algebra) andThen scheme.cata(JsonSchema.requiredFields.algebra)
      val jsonSchema = transformation(SupportedTypes.json.apply[ToJsonSchema].schema)
      val json = JsonSchema.toJson(jsonSchema)
      val expected = J.obj(
        "type" -> J.fromString("object"),
        "properties" -> J.obj(
          "either" -> J.obj(
            "anyOf" -> J.arr(
              List(
                J.obj(
                  "exclusiveMaximum" -> J.fromBoolean(false),
                  "type"             -> J.fromString("integer"),
                  "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
                  "exclusiveMinimum" -> J.fromBoolean(false),
                  "format"           -> J.fromString("int32"),
                  "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
                ),
                J.obj("type" -> J.fromString("string"))
              )
            )
          ),
          "vector" -> J.obj(
            "type"        -> J.fromString("array"),
            "uniqueItems" -> J.fromBoolean(false),
            "items" -> J.obj(
              "exclusiveMaximum" -> J.fromBoolean(false),
              "type"             -> J.fromString("integer"),
              "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
              "exclusiveMinimum" -> J.fromBoolean(false),
              "format"           -> J.fromString("int32"),
              "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
            )
          ),
          "boolean" -> J.obj("type" -> J.fromString("boolean")),
          "nel" -> J.obj(
            "type"        -> J.fromString("array"),
            "uniqueItems" -> J.fromBoolean(false),
            "items" -> J.obj(
              "exclusiveMaximum" -> J.fromBoolean(false),
              "type"             -> J.fromString("integer"),
              "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
              "exclusiveMinimum" -> J.fromBoolean(false),
              "format"           -> J.fromString("int32"),
              "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
            ),
            "minItems" -> J.fromBigDecimal(BigDecimal("1"))
          ),
          "int" -> J.obj(
            "exclusiveMaximum" -> J.fromBoolean(false),
            "type"             -> J.fromString("integer"),
            "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
            "exclusiveMinimum" -> J.fromBoolean(false),
            "format"           -> J.fromString("int32"),
            "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
          ),
          "userId" -> J.obj(
            "exclusiveMaximum" -> J.fromBoolean(false),
            "type"             -> J.fromString("integer"),
            "maximum"          -> J.fromBigDecimal(BigDecimal("9223372036854775807")),
            "exclusiveMinimum" -> J.fromBoolean(false),
            "format"           -> J.fromString("int64"),
            "minimum"          -> J.fromBigDecimal(BigDecimal("-9223372036854775808"))
          ),
          "color" -> J.obj("type" -> J.fromString("string")),
          "set" -> J.obj(
            "type"        -> J.fromString("array"),
            "uniqueItems" -> J.fromBoolean(true),
            "items" -> J.obj(
              "exclusiveMaximum" -> J.fromBoolean(false),
              "type"             -> J.fromString("integer"),
              "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
              "exclusiveMinimum" -> J.fromBoolean(false),
              "format"           -> J.fromString("int32"),
              "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
            )
          ),
          "float" -> J.obj(
            "exclusiveMaximum" -> J.fromBoolean(false),
            "type"             -> J.fromString("number"),
            "maximum"          -> J.fromBigDecimal(BigDecimal("3.4028234663852886E+38")),
            "exclusiveMinimum" -> J.fromBoolean(false),
            "format"           -> J.fromString("float"),
            "minimum"          -> J.fromBigDecimal(BigDecimal("-3.4028234663852886E+38"))
          ),
          "bookingProcess" -> J.obj(
            "anyOf" -> J.arr(
              List(
                J.obj(
                  "type" -> J.fromString("object"),
                  "properties" -> J.obj(
                    "reason" -> J.obj("type" -> J.fromString("string")),
                    "at" -> J.obj(
                      "exclusiveMaximum" -> J.fromBoolean(false),
                      "type"             -> J.fromString("integer"),
                      "maximum"          -> J.fromBigDecimal(BigDecimal("9223372036854775807")),
                      "exclusiveMinimum" -> J.fromBoolean(false),
                      "format"           -> J.fromString("int64"),
                      "minimum"          -> J.fromBigDecimal(BigDecimal("-9223372036854775808"))
                    )
                  ),
                  "required" -> J.arr(List(J.fromString("reason"), J.fromString("at")))
                ),
                J.obj("type" -> J.fromString("object"), "properties" -> J.obj("name" -> J.obj("type" -> J.fromString("string"))), "required" -> J.arr(List(J.fromString("name")))),
                J.obj(
                  "type" -> J.fromString("object"),
                  "properties" -> J.obj(
                    "name" -> J.obj(
                      "exclusiveMaximum" -> J.fromBoolean(false),
                      "type"             -> J.fromString("integer"),
                      "maximum"          -> J.fromBigDecimal(BigDecimal("9223372036854775807")),
                      "exclusiveMinimum" -> J.fromBoolean(false),
                      "format"           -> J.fromString("int64"),
                      "minimum"          -> J.fromBigDecimal(BigDecimal("-9223372036854775808"))
                    )
                  ),
                  "required" -> J.arr(List(J.fromString("name")))
                )
              )
            )
          ),
          "option" -> J.obj(
            "exclusiveMaximum" -> J.fromBoolean(false),
            "type"             -> J.fromString("integer"),
            "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
            "exclusiveMinimum" -> J.fromBoolean(false),
            "format"           -> J.fromString("int32"),
            "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
          ),
          "double" -> J.obj(
            "exclusiveMaximum" -> J.fromBoolean(false),
            "type"             -> J.fromString("number"),
            "maximum"          -> J.fromBigDecimal(BigDecimal("1.7976931348623157E+308")),
            "exclusiveMinimum" -> J.fromBoolean(false),
            "format"           -> J.fromString("double"),
            "minimum"          -> J.fromBigDecimal(BigDecimal("-1.7976931348623157E+308"))
          ),
          "string" -> J.obj("type" -> J.fromString("string")),
          "long" -> J.obj(
            "exclusiveMaximum" -> J.fromBoolean(false),
            "type"             -> J.fromString("integer"),
            "maximum"          -> J.fromBigDecimal(BigDecimal("9223372036854775807")),
            "exclusiveMinimum" -> J.fromBoolean(false),
            "format"           -> J.fromString("int64"),
            "minimum"          -> J.fromBigDecimal(BigDecimal("-9223372036854775808"))
          ),
          "seq" -> J.obj(
            "type"        -> J.fromString("array"),
            "uniqueItems" -> J.fromBoolean(false),
            "items" -> J.obj(
              "exclusiveMaximum" -> J.fromBoolean(false),
              "type"             -> J.fromString("integer"),
              "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
              "exclusiveMinimum" -> J.fromBoolean(false),
              "format"           -> J.fromString("int32"),
              "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
            )
          ),
          "list" -> J.obj(
            "type"        -> J.fromString("array"),
            "uniqueItems" -> J.fromBoolean(false),
            "items" -> J.obj(
              "exclusiveMaximum" -> J.fromBoolean(false),
              "type"             -> J.fromString("integer"),
              "maximum"          -> J.fromBigDecimal(BigDecimal("2147483647")),
              "exclusiveMinimum" -> J.fromBoolean(false),
              "format"           -> J.fromString("int32"),
              "minimum"          -> J.fromBigDecimal(BigDecimal("-2147483648"))
            )
          )
        ),
        "required" -> J.arr(
          List(
            J.fromString("seq"),
            J.fromString("long"),
            J.fromString("vector"),
            J.fromString("int"),
            J.fromString("float"),
            J.fromString("nel"),
            J.fromString("string"),
            J.fromString("either"),
            J.fromString("bookingProcess"),
            J.fromString("color"),
            J.fromString("double"),
            J.fromString("set"),
            J.fromString("boolean"),
            J.fromString("userId"),
            J.fromString("list")
          )
        )
      )

      println(scheme.cata(assertionPrinter).apply(json))

      json must beEqualTo(expected)

    }

    "io.swagger OpenAPIV3Parser should be able to parse our definition" >> {

      val json = Printer.noSpaces.copy(dropNullValues = true).pretty(openApiToJson(spec))
      val parser = new OpenAPIV3Parser()
      val openAPI = parser.readContents(json)

      openAPI.getOpenAPI.getInfo.getDescription must beEqualTo("This is a test api")
      openAPI.getOpenAPI.getInfo.getTitle must beEqualTo("Test API")
      openAPI.getOpenAPI.getInfo.getVersion must beEqualTo("1.0")

    }
  }

  val assertionPrinter: Algebra[JsonF, String] = Algebra {
    case JsonF.Object(fields) => s"J.obj(${fields.map { case (key, value) => s""""$key"""" -> value }.mkString(",")})"
    case JsonF.Null           => "J.nil"
    case JsonF.Str(value)     => s"""J.fromString("$value")"""
    case JsonF.Bool(value)    => s"J.fromBoolean($value)"
    case JsonF.Number(value)  => s"""J.fromBigDecimal(BigDecimal("$value"))"""
    case JsonF.Array(values)  => s"J.arr(List(${values.mkString(",")}))"
  }
}

object OpenApiEndpoints extends OpenApiGen with Endpoints with OpenApiGenJson

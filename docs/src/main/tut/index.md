---
layout: home
title:  "Home"
section: "home"
---

[![Build Status](https://api.travis-ci.org/vectos/itinere.svg)](https://travis-ci.org/vectos/formulation)
[![codecov.io](http://codecov.io/github/vectos/itinere/coverage.svg?branch=master)](http://codecov.io/github/vectos/itinere?branch=master)

# Minimal example

### Imports

```tut:silent
import cats.implicits._
import cats.effect.{IO, Effect}
import cats.Show
```

### Protocol definition

This is typically defined in a module with minimal dependencies like only the domain, the `itinere-core` and `itinere-refined` modules.

```tut:silent
import itinere._

sealed trait DomainResponse[+A]
object DomainResponse {
  final case class Success[A](value: A) extends DomainResponse[A]
  final case class BadRequest(error: String) extends DomainResponse[Nothing]
  final case class NotFound(error: String) extends DomainResponse[Nothing]

  def success[A](value: Json[A]): Json[Success[A]] = Json.object1("Success")(Success[A](_))("value" -> member(value, _.value))
  val badRequest: Json[BadRequest] = Json.object1("BadRequest")(BadRequest.apply)("error" -> member(Json.string, _.error))
  val notFound: Json[NotFound] = Json.object1("NotFound")(NotFound.apply)("error" -> member(Json.string, _.error))
}

final case class User(id: Long, name: String, age: Int)

trait Endpoints extends HttpEndpointAlgebra with HttpJsonAlgebra {

  val userJson: Json[User] = Json.object3("User")(User.apply)(
    "id"   -> member(Json.long, _.id),
    "name" -> member(Json.string, _.name),
    "age"  -> member(Json.int, _.age)
  )

  val userGet =
    endpoint(
      request(HttpMethod.GET, path / "users" / segment("userId", _.long)),
      domainResponse(userJson),
      EndpointCategory.users,
      "Endpoint for getting a specific users"
    )

  def domainResponse[A](value: Json[A]): HttpResponse[DomainResponse[A]] =
    anyOf
      .opt(response(HttpStatus.Ok, "Response in case of success", entity = jsonResponse(DomainResponse.success(value))))
      .opt(response(HttpStatus.NotFound, "Response in case of the entity was not found", entity = jsonResponse(DomainResponse.notFound)))
      .opt(response(HttpStatus.BadRequest, "Response in case of the request body or parameters were in correct", entity = jsonResponse(DomainResponse.badRequest)))
      .as[DomainResponse[A]]

  sealed abstract class EndpointCategory(val value: String)
  object EndpointCategory {
    case object Users extends EndpointCategory("Users")

    def users: EndpointCategory = Users

    implicit val show: Show[EndpointCategory] = Show.show(_.value)
  }
}
```

### Http4s server

This uses the protocol definition to implement a type-safe server, to encode/decode HTTP/JSON as described in the protocol definition, thus removing the overhead to do it yourself.

```tut:silent
import itinere.circe.CirceJsonLike
import itinere.http4s_server.{Http4sServer, Http4sServerJson}
import org.http4s._

object EndpointsHttp4sServer extends Http4sServer with Endpoints with Http4sServerJson with CirceJsonLike {
  override type F[A] = IO[A]
  override def F: Effect[IO] = Effect[IO]

  val routes =
    userGet.implementedBy { case uid => IO.pure(if (uid == 1l) DomainResponse.Success(User(uid, "Klaas", 3)) else DomainResponse.NotFound("User was not found")) }
}
```

Now lets use our server!

```tut:silent
import org.http4s.implicits._

val request = IO.pure(Request[IO](Method.GET, Uri.uri("/users/1")))
val router = EndpointsHttp4sServer.routes.orNotFound
```

```tut:silent
val resp = request.flatMap(router.run).unsafeRunSync()
```

```tut
resp.status
resp.as[String].unsafeRunSync()
```

### Openapi definition

```tut:silent
import itinere.openapi.{OpenApiGen, OpenApiGenJson, OpenApiInfo, OpenApiRoot, OpenApiServer}

object OpenApiEndpoints extends OpenApiGen with Endpoints with OpenApiGenJson

val api = OpenApiEndpoints
val apiServers = Set(OpenApiServer("http://localhost:8080"))
val apiInfo = OpenApiInfo("Test API", "1.0", "This is a test api")
val spec = OpenApiGen.api(apiInfo, apiServers)(api.userGet)
```

```tut:silent
import io.circe.Printer
import itinere.openapi.circe.openApiToJson
```

You could paste the following JSON in [http://editor.swagger.io](http://editor.swagger.io)

```tut
Printer.spaces2.copy(dropNullValues = true).pretty(openApiToJson(spec))
```




package itinere.openapi

import itinere.{HttpMethods, HttpStatusCodes, Primitives}

object OpenApiTypes extends Primitives[Lambda[A => OpenApiType]] {
  override def string: OpenApiType = OpenApiType.String
  override def int: OpenApiType = OpenApiType.Integer
  override def long: OpenApiType = OpenApiType.Integer
}

object OpenApiMethods extends HttpMethods[String] {
  val ACL: String = "ACL"
  val `BASELINE-CONTROL`: String = "BASELINE-CONTROL"
  val BIND: String = "BIND"
  val CHECKIN: String = "CHECKIN"
  val CHECKOUT: String = "CHECKOUT"
  val CONNECT: String = "CONNECT"
  val COPY: String = "COPY"
  val DELETE: String = "DELETE"
  val GET: String = "GET"
  val HEAD: String = "HEAD"
  val LABEL: String = "LABEL"
  val LINK: String = "LINK"
  val LOCK: String = "LOCK"
  val MERGE: String = "MERGE"
  val MKACTIVITY: String = "MKACTIVITY"
  val MKCALENDAR: String = "MKCALENDAR"
  val MKCOL: String = "MKCOL"
  val MKREDIRECTREF: String = "MKREDIRECTREF"
  val MKWORKSPACE: String = "MKWORKSPACE"
  val MOVE: String = "MOVE"
  val OPTIONS: String = "OPTIONS"
  val ORDERPATCH: String = "ORDERPATCH"
  val PATCH: String = "PATCH"
  val POST: String = "POST"
  val PROPFIND: String = "PROPFIND"
  val PROPPATCH: String = "PROPPATCH"
  val PUT: String = "PUT"
  val REBIND: String = "REBIND"
  val REPORT: String = "REPORT"
  val SEARCH: String = "SEARCH"
  val TRACE: String = "TRACE"
  val UNBIND: String = "UNBIND"
  val UNCHECKOUT: String = "UNCHECKOUT"
  val UNLINK: String = "UNLINK"
  val UNLOCK: String = "UNLOCK"
  val UPDATE: String = "UPDATE"
  val UPDATEREDIRECTREF: String = "UPDATEREDIRECTREF"
  val `VERSION-CONTROL`: String = "VERSION-CONTROL"
}

object OpenApiStatusCodes extends HttpStatusCodes[Int] {
  val Continue: Int = 100
  val SwitchingProtocols: Int = 101
  val Processing: Int = 102
  val EarlyHints: Int = 103
  val Ok: Int = 200
  val Created: Int = 201
  val Accepted: Int = 202
  val NonAuthoritativeInformation: Int = 203
  val NoContent: Int = 204
  val ResetContent: Int = 205
  val PartialContent: Int = 206
  val MultiStatus: Int = 207
  val AlreadyReported: Int = 208
  val IMUsed: Int = 226
  val MultipleChoices: Int = 300
  val MovedPermanently: Int = 301
  val Found: Int = 302
  val SeeOther: Int = 303
  val NotModified: Int = 304
  val UseProxy: Int = 305
  val TemporaryRedirect: Int = 307
  val PermanentRedirect: Int = 308
  val BadRequest: Int = 400
  val Unauthorized: Int = 401
  val PaymentRequired: Int = 402
  val Forbidden: Int = 403
  val NotFound: Int = 404
  val MethodNotAllowed: Int = 405
  val NotAcceptable: Int = 406
  val ProxyAuthenticationRequired: Int = 407
  val RequestTimeout: Int = 408
  val Conflict: Int = 409
  val Gone: Int = 410
  val LengthRequired: Int = 411
  val PreconditionFailed: Int = 412
  val PayloadTooLarge: Int = 413
  val UriTooLong: Int = 414
  val UnsupportedMediaType: Int = 415
  val RangeNotSatisfiable: Int = 416
  val ExpectationFailed: Int = 417
  val MisdirectedRequest: Int = 421
  val UnprocessableEntity: Int = 422
  val Locked: Int = 423
  val FailedDependency: Int = 424
  val TooEarly: Int = 425
  val UpgradeRequired: Int = 426
  val PreconditionRequired: Int = 428
  val TooManyRequests: Int = 429
  val RequestHeaderFieldsTooLarge: Int = 431
  val UnavailableForLegalReasons: Int = 451
  val InternalServerError: Int = 500
  val NotImplemented: Int = 501
  val BadGateway: Int = 502
  val ServiceUnavailable: Int = 503
  val GatewayTimeout: Int = 504
  val HttpVersionNotSupported: Int = 505
  val VariantAlsoNegotiates: Int = 506
  val InsufficientStorage: Int = 507
  val LoopDetected: Int = 508
  val NotExtended: Int = 510
  val NetworkAuthenticationRequired: Int = 511
}

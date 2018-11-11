package itinere.http4s_server
import itinere.{HttpMethods, HttpStatusCodes}
import org.http4s.{Method, Status}

object Http4sMethods extends HttpMethods[Method] {
  // $COVERAGE-OFF$Too many methods to test
  val ACL: Method = Method.ACL
  val `BASELINE-CONTROL`: Method = Method.`BASELINE-CONTROL`
  val BIND: Method = Method.BIND
  val CHECKIN: Method = Method.CHECKIN
  val CHECKOUT: Method = Method.CHECKOUT
  val CONNECT: Method = Method.CONNECT
  val COPY: Method = Method.COPY
  val DELETE: Method = Method.DELETE
  val GET: Method = Method.GET
  val HEAD: Method = Method.HEAD
  val LABEL: Method = Method.LABEL
  val LINK: Method = Method.LINK
  val LOCK: Method = Method.LOCK
  val MERGE: Method = Method.MERGE
  val MKACTIVITY: Method = Method.MKACTIVITY
  val MKCALENDAR: Method = Method.MKCALENDAR
  val MKCOL: Method = Method.MKCOL
  val MKREDIRECTREF: Method = Method.MKREDIRECTREF
  val MKWORKSPACE: Method = Method.MKWORKSPACE
  val MOVE: Method = Method.MOVE
  val OPTIONS: Method = Method.OPTIONS
  val ORDERPATCH: Method = Method.ORDERPATCH
  val PATCH: Method = Method.PATCH
  val POST: Method = Method.POST
  val PROPFIND: Method = Method.PROPFIND
  val PROPPATCH: Method = Method.PROPPATCH
  val PUT: Method = Method.PUT
  val REBIND: Method = Method.REBIND
  val REPORT: Method = Method.REPORT
  val SEARCH: Method = Method.SEARCH
  val TRACE: Method = Method.TRACE
  val UNBIND: Method = Method.UNBIND
  val UNCHECKOUT: Method = Method.UNCHECKOUT
  val UNLINK: Method = Method.UNLINK
  val UNLOCK: Method = Method.UNLOCK
  val UPDATE: Method = Method.UPDATE
  val UPDATEREDIRECTREF: Method = Method.UPDATEREDIRECTREF
  val `VERSION-CONTROL`: Method = Method.`VERSION-CONTROL`
  // $COVERAGE-ON$
}

object Http4sStatusCodes extends HttpStatusCodes[Status] {
  // $COVERAGE-OFF$Too many codes to test
  val Continue: Status = Status.Continue
  val SwitchingProtocols: Status = Status.SwitchingProtocols
  val Processing: Status = Status.Processing
  val EarlyHints: Status = Status.EarlyHints
  val Ok: Status = Status.Ok
  val Created: Status = Status.Created
  val Accepted: Status = Status.Accepted
  val NonAuthoritativeInformation: Status = Status.NonAuthoritativeInformation
  val NoContent: Status = Status.NoContent
  val ResetContent: Status = Status.ResetContent
  val PartialContent: Status = Status.PartialContent
  val MultiStatus: Status = Status.MultiStatus
  val AlreadyReported: Status = Status.AlreadyReported
  val IMUsed: Status = Status.IMUsed
  val MultipleChoices: Status = Status.MultipleChoices
  val MovedPermanently: Status = Status.MovedPermanently
  val Found: Status = Status.Found
  val SeeOther: Status = Status.SeeOther
  val NotModified: Status = Status.NotModified
  val UseProxy: Status = Status.UseProxy
  val TemporaryRedirect: Status = Status.TemporaryRedirect
  val PermanentRedirect: Status = Status.PermanentRedirect
  val BadRequest: Status = Status.BadRequest
  val Unauthorized: Status = Status.Unauthorized
  val PaymentRequired: Status = Status.PaymentRequired
  val Forbidden: Status = Status.Forbidden
  val NotFound: Status = Status.NotFound
  val MethodNotAllowed: Status = Status.MethodNotAllowed
  val NotAcceptable: Status = Status.NotAcceptable
  val ProxyAuthenticationRequired: Status = Status.ProxyAuthenticationRequired
  val RequestTimeout: Status = Status.RequestTimeout
  val Conflict: Status = Status.Conflict
  val Gone: Status = Status.Gone
  val LengthRequired: Status = Status.LengthRequired
  val PreconditionFailed: Status = Status.PreconditionFailed
  val PayloadTooLarge: Status = Status.PayloadTooLarge
  val UriTooLong: Status = Status.UriTooLong
  val UnsupportedMediaType: Status = Status.UnsupportedMediaType
  val RangeNotSatisfiable: Status = Status.RangeNotSatisfiable
  val ExpectationFailed: Status = Status.ExpectationFailed
  val MisdirectedRequest: Status = Status.MisdirectedRequest
  val UnprocessableEntity: Status = Status.UnprocessableEntity
  val Locked: Status = Status.Locked
  val FailedDependency: Status = Status.FailedDependency
  val TooEarly: Status = Status.TooEarly
  val UpgradeRequired: Status = Status.UpgradeRequired
  val PreconditionRequired: Status = Status.PreconditionRequired
  val TooManyRequests: Status = Status.TooManyRequests
  val RequestHeaderFieldsTooLarge: Status = Status.RequestHeaderFieldsTooLarge
  val UnavailableForLegalReasons: Status = Status.UnavailableForLegalReasons
  val InternalServerError: Status = Status.InternalServerError
  val NotImplemented: Status = Status.NotImplemented
  val BadGateway: Status = Status.BadGateway
  val ServiceUnavailable: Status = Status.ServiceUnavailable
  val GatewayTimeout: Status = Status.GatewayTimeout
  val HttpVersionNotSupported: Status = Status.HttpVersionNotSupported
  val VariantAlsoNegotiates: Status = Status.VariantAlsoNegotiates
  val InsufficientStorage: Status = Status.InsufficientStorage
  val LoopDetected: Status = Status.LoopDetected
  val NotExtended: Status = Status.NotExtended
  val NetworkAuthenticationRequired: Status = Status.NetworkAuthenticationRequired
  // $COVERAGE-ON$
}

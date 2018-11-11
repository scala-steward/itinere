package itinere
import itinere.circe._
import org.scalacheck.{Arbitrary, Gen}
import org.specs2.ScalaCheck
import org.specs2.matcher.Matchers
import org.specs2.mutable.Specification

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

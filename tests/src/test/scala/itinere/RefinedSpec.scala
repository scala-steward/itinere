package itinere
import eu.timepit.refined._
import io.circe.DecodingFailure
import io.circe.literal._
import itinere.circe.CirceJsonLike
import itinere.refined._
import org.specs2.matcher.Matchers

class RefinedSpec extends org.specs2.mutable.Specification with Matchers with CirceJsonLike {

  "Refinement types" >> {
    "string" >> {
      "minLength" >> {
        val json = Json.string.minLength[W.`3`.T]

        "error" >> assertError(json, json""""Bo"""", "Predicate taking size(Bo) = 2 failed: Predicate (2 < 3) did not fail.")
        "success" >> assertSuccess(json, json""""Jos"""")(refineMV("Jos"))
      }

      "maxLength" >> {
        val json = Json.string.maxLength[W.`3`.T]

        "error" >> assertError(json, json""""Mark"""", "Predicate taking size(Mark) = 4 failed: Right predicate of (!(4 < 0) && !(4 > 3)) failed: Predicate (4 > 3) did not fail.")
        "success" >> assertSuccess(json, json""""Jos"""")(refineMV("Jos"))
      }

      "sized" >> {
        val json = Json.string.sized[W.`3`.T, W.`5`.T]

        "error - too small" >> assertError(json, json""""Bo"""", "Predicate taking size(Bo) = 2 failed: Left predicate of (!(2 < 3) && !(2 > 5)) failed: Predicate (2 < 3) did not fail.")
        "error - too big" >> assertError(
          json,
          json""""Diederik"""",
          "Predicate taking size(Diederik) = 8 failed: Right predicate of (!(8 < 3) && !(8 > 5)) failed: Predicate (8 > 5) did not fail."
        )
        "success" >> assertSuccess(json, json""""Jos"""")(refineMV("Jos"))
      }

      "regex" >> {
        val json = Json.string.matchesRegex[W.`"([A-Za-z]{3,5})"`.T]

        "error" >> assertError(json, json""""T1"""", """Predicate failed: "T1".matches("([A-Za-z]{3,5})").""")
        "success" >> assertSuccess(json, json""""Mark"""")(refineMV("Mark"))
      }
    }

    "int" >> {
      "negative" >> {
        val json = Json.int.negative

        "error" >> assertError(json, json"1", "Predicate failed: (1 < 0).")
        "success" >> assertSuccess(json, json"-1")(refineMV(-1))
      }

      "positive" >> {
        val json = Json.int.positive

        "error" >> assertError(json, json"-1", "Predicate failed: (-1 > 0).")
        "success" >> assertSuccess(json, json"1")(refineMV(1))
      }

      "intervalOpen" >> {
        val json = Json.int.intervalOpen[W.`3`.T, W.`5`.T]

        "error" >> assertError(json, json"3", "Left predicate of ((3 > 3) && (3 < 5)) failed: Predicate failed: (3 > 3).")
        "success" >> assertSuccess(json, json"4")(refineMV(4))
      }

      "intervalClosedOpen" >> {
        val json = Json.int.intervalClosedOpen[W.`3`.T, W.`5`.T]

        "error - too small" >> assertError(json, json"2", "Left predicate of (!(2 < 3) && (2 < 5)) failed: Predicate (2 < 3) did not fail.")
        "error - too big" >> assertError(json, json"5", "Right predicate of (!(5 < 3) && (5 < 5)) failed: Predicate failed: (5 < 5).")
        "success" >> assertSuccess(json, json"3")(refineMV(3))
      }

      "intervalClosedOpen" >> {
        val json = Json.int.intervalOpenClosed[W.`3`.T, W.`5`.T]

        "error - too small" >> assertError(json, json"2", "Left predicate of ((2 > 3) && !(2 > 5)) failed: Predicate failed: (2 > 3).")
        "error - too big" >> assertError(json, json"6", "Right predicate of ((6 > 3) && !(6 > 5)) failed: Predicate (6 > 5) did not fail.")
        "success" >> assertSuccess(json, json"4")(refineMV(4))
      }

      "intervalClosed" >> {
        val json = Json.int.intervalClosed[W.`3`.T, W.`5`.T]

        "error - too small" >> assertError(json, json"2", "Left predicate of (!(2 < 3) && !(2 > 5)) failed: Predicate (2 < 3) did not fail.")
        "error - too big" >> assertError(json, json"6", "Right predicate of (!(6 < 3) && !(6 > 5)) failed: Predicate (6 > 5) did not fail.")
        "success" >> assertSuccess(json, json"3")(refineMV(3))
      }
    }
  }

  private def assertError[A](descriptor: Json[A], value: io.circe.Json, error: String) =
    fromJson(descriptor).decode(value.noSpaces) must beEqualTo(Attempt.error("Json decode error", Some(DecodingFailure(error, Nil))))

  private def assertSuccess[A](descriptor: Json[A], value: io.circe.Json)(expected: A) =
    fromJson(descriptor).decode(value.noSpaces) must beEqualTo(Attempt.success(expected))

  def fromJson[A](descriptor: Json[A]): FromJson[A] = jsonDecoder(descriptor)

}

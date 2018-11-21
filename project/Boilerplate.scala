import sbt._

object Boilerplate {

  import scala.StringContext._

  implicit final class BlockHelper(val sc: StringContext) extends AnyVal {
    def block(args: Any*): String = {
      val interpolated = sc.standardInterpolator(treatEscapes, args)
      val rawLines = interpolated split '\n'
      val trimmedLines = rawLines map {
        _ dropWhile (_.isWhitespace)
      }
      trimmedLines mkString "\n"
    }
  }

  val coreTemplates: Seq[Template] = Seq(
    GenJasonAlgebra,
    GenJasonDsl
  )

  val circeTemplates: Seq[Template] = Seq(
    GenCirceEncoder,
    GenCirceDecoder
  )

  val openapiTemplates: Seq[Template] = Seq(GenToJsonSchemaFormatN)

  val header = "// auto-generated boilerplate" // TODO: put something meaningful here?

  def gen(templates: Seq[Template])(dir: File) = for (t <- templates) yield {
    val tgtFile = t.filename(dir)
    IO.write(tgtFile, t.body)
    tgtFile
  }

  val maxArity = 22

  final class TemplateVals(val arity: Int) {
    val synTypes = (0 until arity) map (n => s"A$n")
    val synVals = (0 until arity) map (n => s"a$n")
    val synTypedVals = (synVals zip synTypes) map { case (v, t) => v + ":" + t }
    val `A..N` = synTypes.mkString(", ")
    val `a..n` = synVals.mkString(", ")
    val `_.._` = Seq.fill(arity)("_").mkString(", ")
    val `(A..N)` = if (arity == 1) "Tuple1[A]" else synTypes.mkString("(", ", ", ")")
    val `(_.._)` = if (arity == 1) "Tuple1[_]" else Seq.fill(arity)("_").mkString("(", ", ", ")")
    val `(a..n)` = if (arity == 1) "Tuple1(a)" else synVals.mkString("(", ", ", ")")
    val `a:A..n:N` = synTypedVals mkString ", "
  }

  trait Template {
    def filename(root: File): File

    def content(tv: TemplateVals): String

    def range = 1 to maxArity

    def body: String = {
      def expandInstances(contents: IndexedSeq[Array[String]], acc: Array[String] = Array.empty): Array[String] =
        if (!contents.exists(_ exists (_ startsWith "-")))
          acc map (_.tail)
        else {
          val pre = contents.head takeWhile (_ startsWith "|")
          val instances = contents flatMap {
            _ dropWhile (_ startsWith "|") takeWhile (_ startsWith "-")
          }
          val next = contents map {
            _ dropWhile (_ startsWith "|") dropWhile (_ startsWith "-")
          }
          expandInstances(next, acc ++ pre ++ instances)
        }

      val rawContents = range map { n =>
        content(new TemplateVals(n)) split '\n' filterNot (_.isEmpty)
      }
      val headerLines = header split '\n'
      val instances = expandInstances(rawContents)
      val footerLines = rawContents.head.reverse.takeWhile(_ startsWith "|").map(_.tail).reverse
      (headerLines ++ instances ++ footerLines) mkString "\n"
    }
  }

  object GenJasonAlgebra extends Template {
    def filename(root: File) = root / "itinere" / "JsonAlgebraObjectN.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val params = synTypes map { tpe =>
        s"param$tpe: (String, Member[F, $tpe, Z])"
      } mkString ", "

      block"""
             |package itinere
             |
        |trait JsonAlgebraFormatN[F[_]] {
        -  def object${arity}[${`A..N`}, Z](name: String)(f: (${`A..N`}) => Z)($params): F[Z]
        -  def discriminated${arity}[${`A..N`}, Z](objectName: String, discriminatorField: String)(f: (${`A..N`}) => Z)($params): F[Z]
             |}
      """
    }
  }

  object GenJasonDsl extends Template {
    def filename(root: File) = root / "itinere" / "JsonDslObjectN.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val params = synTypes map { tpe =>
        s"param$tpe: (String, Member[Json, $tpe, Z])"
      } mkString ", "
      val applies = synTypes map { tpe =>
        s"param$tpe._1 -> param$tpe._2.transform(naturalTransformation)"
      } mkString ", "

      block"""
             |package itinere
             |
        |import cats.~>
             |
        |trait JsonDslFormatN {
             |  private def naturalTransformation[G[_] : JsonAlgebra]: (Json ~> G) = new (Json ~> G) {
             |    override def apply[A](fa: Json[A]): G[A] = fa.apply[G]
             |  }
        -  def object$arity[${`A..N`}, Z](name: String)(f: (${`A..N`}) => Z)($params): Json[Z] = new Json[Z] { def apply[F[_] : JsonAlgebra]: F[Z] = implicitly[JsonAlgebra[F]].object$arity(name)(f)($applies) }
        -  def discriminated$arity[${`A..N`}, Z](objectName: String, discriminatorField: String)(f: (${`A..N`}) => Z)($params): Json[Z] = new Json[Z] { def apply[F[_] : JsonAlgebra]: F[Z] = implicitly[JsonAlgebra[F]].discriminated$arity(objectName, discriminatorField)(f)($applies) }
             |}
             |
      """
    }
  }

  object GenCirceEncoder extends Template {
    def filename(root: File) = root / "itinere" / "circe" / "CirceEncoderObjectN.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val params = synTypes map { tpe =>
        s"param$tpe: (String, Member[Encoder, $tpe, Z])"
      } mkString ", "
      val applies = synTypes map { tpe =>
        s"param$tpe._1 -> param$tpe._2.fa(param$tpe._2.getter(v))"
      } mkString ", "

      block"""
             |package itinere.circe
             |
             |import com.github.ghik.silencer.silent
             |import io.circe.{Encoder, Json}
             |import itinere._
             |
             |@silent
             |trait CirceEncoderObjectN { self: JsonAlgebra[Encoder] =>
             -  def object$arity[${`A..N`}, Z](name: String)(f: (${`A..N`}) => Z)($params): Encoder[Z] = new Encoder[Z] { def apply(v: Z): Json = { Json.obj($applies) } }
             -  def discriminated$arity[${`A..N`}, Z](objectName: String, discriminatorField: String)(f: (${`A..N`}) => Z)($params): Encoder[Z] = new Encoder[Z] { def apply(v: Z): Json = { Json.obj(discriminatorField -> Json.fromString(objectName), $applies) } }
             |}
             |
      """
    }
  }

  object GenCirceDecoder extends Template {
    def filename(root: File) = root / "itinere" / "circe" / "CirceDecoderObjectN.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val params = synTypes map { tpe =>
        s"param$tpe: (String, Member[Decoder, $tpe, Z])"
      } mkString ", "
      val applies = synTypes map { tpe =>
        s"param$tpe._1"
      } mkString ", "
      val implicits = synTypes map { tpe =>
        s"param$tpe._2.fa"
      } mkString ", "

      block"""
             |package itinere.circe
             |
             |import com.github.ghik.silencer.silent
             |import io.circe.{Decoder, DecodingFailure}
             |import itinere._
             |import cats.implicits._
             |
             |@silent
             |trait CirceDecoderObjectN { self: JsonAlgebra[Decoder] =>
             |
             |  private def disc(field: String, value: String) =
             |    Decoder.instance(_.downField(field).as[String].flatMap(d => if(d.equalsIgnoreCase(value)) Right(()) else Left(DecodingFailure(s"Did not match expected discriminator", Nil))))
             |
             -  def object$arity[${`A..N`}, Z](name: String)(f: (${`A..N`}) => Z)($params): Decoder[Z] = Decoder.forProduct$arity($applies)(f)($implicits)
             -  def discriminated$arity[${`A..N`}, Z](objectName: String, discriminatorField: String)(f: (${`A..N`}) => Z)($params): Decoder[Z] = disc(discriminatorField, objectName) *> Decoder.forProduct$arity($applies)(f)($implicits)
             |}
             |
      """
    }
  }

  object GenToJsonSchemaFormatN extends Template {
    def filename(root: File) = root / "itinere" / "openapi" / "ToJsonSchemaFormatN.scala"

    def content(tv: TemplateVals) = {
      import tv._

      val params = synTypes map { tpe =>
        s"param$tpe: (String, Member[ToJsonSchema, $tpe, Z])"
      } mkString ", "
      val applies = synTypes map { tpe =>
        s"param$tpe._1 -> param$tpe._2.fa.schema"
      } mkString ", "

      block"""
             |package itinere.openapi
             |
             |import com.github.ghik.silencer.silent
             |import itinere._
             |
             |@silent
             |trait ToJsonSchemaFormatN { self: JsonAlgebra[ToJsonSchema] =>
             -  def object$arity[${`A..N`}, Z](name: String)(f: (${`A..N`}) => Z)($params):ToJsonSchema[Z] = ToJsonSchema.create(JsonSchema.obj(name, Map($applies)))
             -  def discriminated$arity[${`A..N`}, Z](objectName: String, discriminatorField: String)(f: (${`A..N`}) => Z)($params):ToJsonSchema[Z] = ToJsonSchema.create(JsonSchema.obj(objectName, Map(discriminatorField -> JsonSchema.enum(Set(objectName)), $applies)))
             |}
             |
      """
    }
  }
}

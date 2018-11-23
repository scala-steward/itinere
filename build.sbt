import microsites.ExtraMdFileConfig

val catsVersion = "1.4.0"
val shapelessVersion = "2.3.3"

val core = project
  .in(file("core"))
  .settings(commonSettings("core"))
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.typelevel" %% "cats-core" % catsVersion,
      "com.chuusai"   %% "shapeless" % shapelessVersion,
    ),
    coverageExcludedPackages := "itinere.*FormatN",
    sourceGenerators in Compile += (sourceManaged in Compile)
      .map(Boilerplate.gen(Boilerplate.coreTemplates))
      .taskValue
  )

val refined = project
  .in(file("refined"))
  .settings(commonSettings("refined"))
  .settings(publishSettings)
  .settings(libraryDependencies ++= Seq("eu.timepit" %% "refined" % "0.9.3"))
  .dependsOn(core)

val `http4s-server` = project
  .in(file("http4s-server"))
  .settings(commonSettings("http4s-server"))
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "org.http4s" %% "http4s-blaze-server" % "0.20.0-M3"
    )
  )
  .dependsOn(core)

val circe = project
  .in(file("circe"))
  .settings(commonSettings("circe"))
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core"   % "0.10.1",
      "io.circe" %% "circe-parser" % "0.10.1"
    ),
    coverageExcludedPackages := "itinere.circe.*ObjectN",
    sourceGenerators in Compile += (sourceManaged in Compile)
      .map(Boilerplate.gen(Boilerplate.circeTemplates))
      .taskValue
  )
  .dependsOn(core)

val `openapi` = project
  .in(file("open-api"))
  .settings(commonSettings("open-api"))
  .settings(publishSettings)
  .settings(
    libraryDependencies ++= Seq(
      "io.higherkindness" %% "droste-core" % "0.5.0"
    ),
    coverageExcludedPackages := "itinere.openapi.*FormatN",
    sourceGenerators in Compile += (sourceManaged in Compile)
      .map(Boilerplate.gen(Boilerplate.openapiTemplates))
      .taskValue
  )
  .dependsOn(core)

val `openapi-circe` = project
  .in(file("open-api-circe"))
  .settings(commonSettings("open-api-circe"))
  .settings(
    libraryDependencies ++= Seq(
      "io.circe" %% "circe-core" % "0.10.1"
    ),
  )
  .settings(publishSettings)
  .dependsOn(`openapi`)

val tests = project
  .in(file("tests"))
  .settings(noPublishSettings)
  .settings(commonSettings("tests"))
  .settings(
    parallelExecution in Test := false,
    libraryDependencies ++= Seq(
      "io.swagger.parser.v3" % "swagger-parser"     % "2.0.5"                       % Test,
      "org.typelevel"        %% "cats-laws"         % catsVersion                   % Test,
      "org.typelevel"        %% "discipline"        % "0.10.0"                      % Test,
      "com.propensive"       %% "magnolia"          % "0.10.0"                      % Test,
      "org.http4s"           %% "http4s-circe"      % "0.20.0-M3"                   % Test,
      "org.http4s"           %% "http4s-dsl"        % "0.20.0-M3"                   % Test,
      "io.circe"             %% "circe-literal"     % "0.10.0"                      % Test,
      "org.specs2"           %% "specs2-core"       % "4.3.4"                       % Test,
      "org.specs2"           %% "specs2-scalacheck" % "4.3.4"                       % Test,
      "org.specs2"           %% "specs2-cats"       % "4.3.5-78abffa2e-20181150936" % Test,
      "org.scalacheck"       %% "scalacheck"        % "1.13.5"                      % Test
    )
  )
  .dependsOn(core, `http4s-server`, circe, refined, `openapi`, `openapi-circe`)

val docs = project
  .in(file("docs"))
  .settings(noPublishSettings)
  .settings(commonSettings("docs"))
  .settings(
    scalacOptions := Seq("-language:higherKinds"),
    micrositeName := "itinere",
    micrositeDescription := "Define endpoints",
    micrositeBaseUrl := "/itinere",
    micrositeDocumentationUrl := "/itinere/docs/",
    micrositeGitterChannel := false,
    micrositeGithubOwner := "vectos",
    micrositeGithubRepo := "itinere",
    micrositeGithubToken := sys.env.get("GITHUB_TOKEN"),
    micrositeOrganizationHomepage := "http://vectos.net",
    micrositeAuthor := "Vectos",
    micrositeTwitterCreator := "@mark_dj"
  )
  .dependsOn(core, refined)
  .enablePlugins(MicrositesPlugin)

lazy val noPublishSettings = Seq(
  publish := {},
  publishLocal := {},
  publishArtifact := false,
  skip in publish := true
)

def commonSettings(n: String) = Seq(
  name := s"itinere-$n",
  organization := "net.vectos",
  scalaVersion := "2.12.7",
  scalafmtOnCompile := true,
  wartremoverErrors ++= Warts.unsafe,
  libraryDependencies ++= Seq(
    compilerPlugin("com.github.ghik" %% "silencer-plugin" % "1.2.1"),
    "com.github.ghik" %% "silencer-lib" % "1.2.1" % Provided
  ),
  scalacOptions := Seq(
    "-deprecation", // Emit warning and location for usages of deprecated APIs.
    "-encoding",
    "utf-8", // Specify character encoding used by source files.
    "-explaintypes", // Explain type errors in more detail.
    "-feature", // Emit warning and location for usages of features that should be imported explicitly.
    "-language:existentials", // Existential types (besides wildcard types) can be written and inferred
    "-language:experimental.macros", // Allow macro definition (besides implementation and application)
    "-language:higherKinds", // Allow higher-kinded types
    "-language:implicitConversions", // Allow definition of implicit functions called views
    "-unchecked", // Enable additional warnings where generated code depends on assumptions.
    "-Xcheckinit", // Wrap field accessors to throw an exception on uninitialized access.
    "-Xfatal-warnings", // Fail the compilation if there are any warnings.
    "-Xfuture", // Turn on future language features.
    "-Xlint:adapted-args", // Warn if an argument list is modified to match the receiver.
    "-Xlint:by-name-right-associative", // By-name parameter of right associative operator.
    "-Xlint:constant", // Evaluation of a constant arithmetic expression results in an error.
    "-Xlint:delayedinit-select", // Selecting member of DelayedInit.
    "-Xlint:doc-detached", // A Scaladoc comment appears to be detached from its element.
    "-Xlint:inaccessible", // Warn about inaccessible types in method signatures.
    "-Xlint:infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Xlint:missing-interpolator", // A string literal appears to be missing an interpolator id.
    "-Xlint:nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Xlint:nullary-unit", // Warn when nullary methods return Unit.
    "-Xlint:option-implicit", // Option.apply used implicit view.
    "-Xlint:package-object-classes", // Class or object defined in package object.
    "-Xlint:poly-implicit-overload", // Parameterized overloaded implicit methods are not visible as view bounds.
    "-Xlint:private-shadow", // A private field (or class parameter) shadows a superclass field.
    "-Xlint:stars-align", // Pattern sequence wildcard must align with sequence component.
    "-Xlint:type-parameter-shadow", // A local type parameter shadows a type already in scope.
    "-Xlint:unsound-match", // Pattern match may not be typesafe.
    "-Yno-adapted-args", // Do not adapt an argument list (either by inserting () or creating a tuple) to match the receiver.
    "-Ypartial-unification", // Enable partial unification in type constructor inference
    "-Ywarn-dead-code", // Warn when dead code is identified.
    "-Ywarn-extra-implicit", // Warn when more than one implicit parameter section is defined.
    "-Ywarn-inaccessible", // Warn about inaccessible types in method signatures.
    "-Ywarn-infer-any", // Warn when a type argument is inferred to be `Any`.
    "-Ywarn-nullary-override", // Warn when non-nullary `def f()' overrides nullary `def f'.
    "-Ywarn-nullary-unit", // Warn when nullary methods return Unit.
    "-Ywarn-numeric-widen", // Warn when numerics are widened.
    "-Ywarn-unused:implicits", // Warn if an implicit parameter is unused.
    "-Ywarn-unused:imports", // Warn if an import selector is not referenced.
    "-Ywarn-unused:locals", // Warn if a local definition is unused.
    "-Ywarn-unused:params", // Warn if a value parameter is unused.
    "-Ywarn-unused:patvars", // Warn if a variable bound in a pattern is unused.
    "-Ywarn-unused:privates", // Warn if a private member is unused.
    "-Ywarn-value-discard" // Warn when non-Unit expression results are unused.
  ),
  scalacOptions in (Compile, console) --= Seq("-Ywarn-unused:imports", "-Xfatal-warnings"),
  addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
)

def publishSettings = Seq(
  publishArtifact in Test := false,
  sources in (Compile, doc) := Seq(),
  licenses += ("MIT", url("http://opensource.org/licenses/MIT")),
  releaseEarlyEnableSyncToMaven := false,
  developers := List(
    Developer(
      "mark_dj",
      "Mark de Jong",
      "mark@vectos.net",
      url("http://vectos.net")
    )
  ),
  homepage := Some(url("https://vectos.net/itinere")),
  scmInfo := Some(
    ScmInfo(
      url("http://github.com/vectos/itinere"),
      "scm:git:git@github.com:vectos/itinere.git"
    )
  ),
  releaseEarlyWith := SonatypePublisher,
  pgpPublicRing := file("./travis/local.pubring.asc"),
  pgpSecretRing := file("./travis/local.secring.asc")
)

def cmdAlias(name: String, commands: List[String]) =
  addCommandAlias(name, s";${commands.mkString(";")}")

val coverageAlias = "clean" :: "coverage" :: "tests/test" :: "coverageReport" :: "coverageAggregate" :: Nil
val testAlias = "clean" :: "tests/test" :: Nil
val scalafmtAlias = "scalafmt" :: "scalafmtSbt" :: Nil
val scalafmtCheckAlias = "scalafmtCheck" :: "scalafmtSbtCheck" :: Nil

cmdAlias("validateTest", testAlias)
cmdAlias("validateCoverage", scalafmtCheckAlias ++ coverageAlias)
cmdAlias("format", scalafmtAlias)

val root = project
  .in(file("."))
  .settings(commonSettings("root") ++ noPublishSettings)
  .aggregate(core, refined, circe, `http4s-server`, `openapi`, `openapi-circe`)

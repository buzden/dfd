name := "discrete-finite-distribution"

version := "0.1"

scalaVersion := "2.12.8"

lazy val catsVersion = "1.6.0"
lazy val specs2Version = "4.4.1"

// General stuff
scalacOptions ++= Seq(
  "-language:higherKinds",
  "-Ypartial-unification",
)
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")

// Data types stuff
libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.16.0" % Test
)

// Testing stuff
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-scalacheck" % specs2Version,
  "io.chrisdavenport" %% "cats-scalacheck" % "0.1.0",
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "discipline" % "0.11.0", // todo to remove as soon as this or newer comes as dep.
).map(_ % Test)
scalacOptions in Test += "-Yrangepos"
logBuffered   in Test := false
testOptions   in Test += Tests.Argument("showtimes")

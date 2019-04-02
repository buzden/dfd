name := "discrete-finite-distribution"

version := "0.1"

val scala2_12_v = "2.12.8"
val scala2_13_v = "2.13.0-M5"

scalaVersion := scala2_12_v
crossScalaVersions := Seq(scala2_13_v, scala2_12_v)

lazy val catsVersion = "1.6.0"
lazy val specs2Version = "4.5.1"

// General stuff
scalacOptions ++= Seq(
  "-language:higherKinds",
)
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")
// Scala version-local stuff
scalacOptions ++= (if (scalaVersion.value.startsWith("2.12.")) Seq("-Ypartial-unification") else Nil)

// Data types stuff
libraryDependencies ++= Seq(
  "org.typelevel" %% "spire" % "0.16.1" % Test
)

// Testing stuff
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-scalacheck" % specs2Version,
  "io.chrisdavenport" %% "cats-scalacheck" % "0.1.1",
  "org.typelevel" %% "cats-laws" % catsVersion,
  "org.typelevel" %% "discipline" % "0.11.0", // todo to remove as soon as this or newer comes as dep.
).map(_ % Test)
scalacOptions in Test += "-Yrangepos"
logBuffered   in Test := false
testOptions   in Test += Tests.Argument("showtimes")

name := "discrete-finite-distribution"

version := "0.1"

scalaVersion := "2.13.0-M5"

lazy val catsVersion = "1.5.0"
lazy val specs2Version = "4.3.6"

// General stuff
scalacOptions ++= Seq(
  "-language:higherKinds",
)
libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
addCompilerPlugin("org.spire-math" %% "kind-projector" % "0.9.9")

// Data types stuff
libraryDependencies ++= Seq(
)

// Testing stuff
libraryDependencies ++= Seq(
  "org.specs2" %% "specs2-scalacheck" % specs2Version % Test,
  "org.typelevel" %% "cats-laws" % catsVersion % Test,
)
scalacOptions in Test += "-Yrangepos"

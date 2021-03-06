name := "discrete-finite-distribution"

version := "0.1"

val scala2_12_v = "2.12.8"
val scala2_13_v = "2.13.0-M5"
val dotty_v     = "0.13.0-RC1"

scalaVersion := scala2_13_v
crossScalaVersions := Seq(scala2_12_v, scala2_13_v, dotty_v)

lazy val catsVersion = "1.6.0"
lazy val specs2Version = "4.5.1"

def scala2Oldy(v: String): Boolean = v.startsWith("2.12.")

// General stuff
scalacOptions ++= { if (isDotty.value) Seq(
    // todo to remove implicitConversions flag as soon as cats are released for dotty.
    "-language:higherKinds,implicitConversions,Scala2",
  ) else Seq(
    "-language:higherKinds",
  )
}
scalacOptions ++= (if (scala2Oldy(scalaVersion.value)) Seq("-Ypartial-unification") else Nil)

libraryDependencies += "org.typelevel" %% "cats-core" % catsVersion
addCompilerPlugin("com.olegpy" %% "better-monadic-for" % "0.3.0-M4")

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
sources       in Test --= {
  if (scala2Oldy(scalaVersion.value) || isDotty.value) Seq(
    baseDirectory.value / "src" / "test" / "scala" / "ru" / "buzden" / "testing" / "util" / "numeric" / "instances.scala",
  ) else Nil
}
unmanagedSourceDirectories in Test ++= {
  if (scala2Oldy(scalaVersion.value) || isDotty.value) Seq(
    baseDirectory.value / "src" / "test" / "scala-oldy",
  ) else Nil
}
// todo to remove `|| isDotty.value` from the above as soon as standard library of 2.13 comes with dotty.

// Dotty support stuff
libraryDependencies := libraryDependencies.value.map(_.withDottyCompat(scalaVersion.value))

val dottyVersion = "3.0.0-RC2"

lazy val root = project
  .in(file("."))
  .settings(
    name := "gaia",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.7",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.7" % "test",
)

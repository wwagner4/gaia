val dottyVersion = "3.0.0-RC3"

lazy val root = project
  .in(file("."))
  .settings(
    name := "gaia",
    version := "0.1.0",

    scalaVersion := dottyVersion,

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.8",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.8" % "test",
    libraryDependencies += "net.entelijan" %% "viz" % "0.2-SNAPSHOT"
  )

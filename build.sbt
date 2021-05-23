lazy val root = project
  .in(file("."))
  .settings(
    name := "gaia",
    version := "0.1.0",

    scalaVersion := "3.0.0",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "net.entelijan" %% "viz" % "0.2-SNAPSHOT"
  )

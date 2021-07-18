lazy val root = project
  .in(file("."))
  .settings(
    name := "gaia",
    version := "0.1.0",

    scalaVersion := "3.0.0",

    libraryDependencies += "org.scalactic" %% "scalactic" % "3.2.9",
    libraryDependencies += "org.scalatest" %% "scalatest" % "3.2.9" % "test",
    libraryDependencies += "net.entelijan" %% "viz" % "0.2-SNAPSHOT",
    libraryDependencies += "net.bramp.ffmpeg" % "ffmpeg" % "0.6.2",
    libraryDependencies += "org.slf4j" % "slf4j-simple" % "1.7.30",
    libraryDependencies += "com.lihaoyi" %% "upickle" % "1.4.0",
    libraryDependencies += "org.scala-lang.modules" %% "scala-parallel-collections" % "1.0.3",
  )

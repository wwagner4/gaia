package gaia

import gaia.Main.RotAxesDeg
import gaia.X3d.{PolarVecDeg, Vec}

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.nio.file.Path
import java.util.function.Consumer
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Tryout {

  def doit(args: List[String], workPath: Path): Unit = {
    println(s"Some tests ${workPath.toAbsolutePath} ${args.mkString("[", ",", "]")}")
    
    println(Util.galacicCenter)
  }

  private def execCmds = {
    val cmds = Seq(
      Seq("wget", "https://google.com"),
      Seq("wget", "http://entelijan.net"),
      Seq("ping", "-c", "5", "entelijan.net"),
      Seq("ping", "-c", "2", "google.com"),
    )

    Util.runAllCommands(cmds)
  }

  private def testRotAxes = {
    val rs = Seq(
      RotAxesDeg(0, 0),
      RotAxesDeg(0, 90),
      RotAxesDeg(90, 0),
    )

    rs.map(r => (r, r.toVec)).foreach { case (a, b) => println(s"$a -> $b") }

    val nearEcl = Vec(0, 45, 30).toPolarVec
    val ra = X3d.radToDeg(nearEcl.ra)
    val dec = X3d.radToDeg(nearEcl.dec)
    println(s"steep: $ra, $dec")
  }
}

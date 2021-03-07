package gaia

import gaia.Data.{Star, readBasic}
import gaia.Main.RotAxesDeg
import gaia.X3d.{PolarVecDeg, Vec}

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.URL
import java.nio.file.Path
import java.util.function.Consumer
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.util.zip.GZIPInputStream
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Tryout {

  def doit(args: List[String], workPath: Path): Unit = {
    println(s"Some tests ${workPath.toAbsolutePath} ${args.mkString("[", ",", "]")}")

    println(ImageUtil.galacicCenter)
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

  private def amalyseStarsPerShell = {
    val shells = Seq(
      (" 1 kpc", 1.0, 1.1),
      (" 2 kpc", 2.0, 2.1),
      (" 3 kpc", 3.0, 3.1),
      (" 4 kpc", 4.0, 4.1),
      (" 5 kpc", 5.0, 5.1),
      (" 6 kpc", 6.0, 6.1),
      (" 7 kpc", 7.0, 7.1),
      (" 8 kpc", 8.0, 8.1),
      (" 9 kpc", 9.0, 9.1),
      ("10 kpc", 10.0, 10.1),
      ("11 kpc", 11.0, 11.1),
      ("12 kpc", 12.0, 12.1),
      ("13 kpc", 13.0, 13.1),
      ("14 kpc", 14.0, 14.1),
    )

    def filterBasic(star: Star)(minDistKpc: Double, maxDistKpc: Double): Option[Star] = {
      if (star.parallax <= 0) return None
      val dist = 1.0 / star.parallax
      if (dist < minDistKpc || dist > maxDistKpc) return None
      return Some(star)
    }

    for ((id, min, max) <- shells) yield {
      val cnt = readBasic.flatMap(filterBasic(_)(min, max)).size
      println(s"$id - $cnt stars")
    }
  }

  private def analyseDataNegativeParallax = {
    var cntAll = 0
    var cntNegPar = 0
    var min = Double.MaxValue
    var max = Double.MinValue
    readBasic.foreach(s => {
      if (s.parallax <= 0) {
        cntNegPar += 1
        if (s.parallax < min) min = s.parallax
        if (s.parallax > max) max = s.parallax
      }
      cntAll += 1
    })
    val minDist = -1 / max
    val maxDist = -1 / min
    val perc = 100.0 * cntNegPar / cntAll
    println("=== negative parallax ===")
    println(f" $cntNegPar of $cntAll are negative parallaxes. $perc%.2f.")
    println(f" min parallax:  $min%20.2f            max parallax: $max%20.2f")
    println(f" max dist:      $minDist%20.3f kpc    min dist:     $maxDist%.3f kpc")
  }

  private def analyseDataBasicSize: Unit = {
    val size = readBasic.size
    println(s"Size of basic is $size")
  }

  private def analyseDataBasicTop: Unit = readBasic.take(20).foreach(println(_))

  private def analyseDataHeader(): Unit = {
    val fn = "GaiaSource_1000172165251650944_1000424567594791808.csv.gz"
    val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/$fn"
    println(s"downloading from $urlStr")
    val header = scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
      .getLines()
      .take(1)
      .toSeq
    header(0)
      .split(",")
      .zipWithIndex
      .foreach { case (nam, i) => println("%4d - %s".format(i, nam)) }
  }
}

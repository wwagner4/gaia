package gaia

import gaia.ImageUtil.StarPosDir
import gaia.X3d.{Color, Shapable}

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.URL
import java.nio.file.Path
import java.util.function.Consumer
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.util.zip.GZIPInputStream
import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.Random

object Tryout {

  import Data.{Star, readBasic}
  import Main.RotAxesDeg
  import Vector._

  def f(prefix: String, a: Double, b: Double, c: Double) = {
    def adj(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

    s"$prefix(%7.4f | %7.4f | %7.4f)".format(adj(a), adj(b), adj(c))
  }

  def f(v: Vec): String = f("C", v.x, v.y, v.z)

  def f(v: PolarVec): String = f("P", v.r, radToDeg(v.ra), radToDeg(v.dec))


  def doit(args: List[String], workPath: Path): Unit = {
    displyDirections()
  }

  private def displyDirections(): Unit = {

    import ImageUtil._
    val bc = Color.veryDarkBlue

    def fromDef = {
      val dirVecs = for (
        vr <- (0 to(340, 20)).map(a => PolarVec(1, degToRad(45), degToRad(a)).toVec);
        ra <- 0 to(350, 45);
        dec <- -80 to(80, 40)) yield {
        val vp = PolarVec(1, degToRad(ra), degToRad(dec)).toVec
        println(s"$vr")
        (vp, vr)
      }
      dirVecs.flatMap { (dv, rot) =>
        Seq(
          X3d.Shapable.Box(position = dv, color = Color.green, size = Vec(0.05, 0.05, 0.05)),
          X3d.Shapable.Cone(position = dv, direction = rot, color = Color.yellow, radius = 0.01),
          X3d.Shapable.Cylinder(position = dv, direction = rot, color = Color.orange, radius = 0.005),
        )
      }
    }

    val shapables = fromDef ++ shapablesCoordinatesColored(2, bc, showSign = true)
    val file = Main.workPath.resolve("tryout_directions.x3d")
    val xml = X3d.createXml(shapables, file.getFileName.toString, bc)
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }

  private def vecConvert: Unit = {


    def adj: Unit = {
      def ad(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

      val y = -0.0
      val x = ad(y)
      val v1 = "%7.2f %7.2f".format(y, x)
      println(v1)
    }

    def norm: Unit = {
      val v = PolarVec(1.0000, 4.1416, 3.1316)
      val vn = v.adjust
      println(s"norm $v -> $vn")
    }

    /*
    [info] - vector convert reconvert pcp PolarVec(6.0,2.0,-3.0) *** FAILED ***
    [info]   "... 6.0000 |  5.1416 | [-]0.1416)" was not equal to "... 6.0000 |  5.1416 | [ ]0.1416)" (Tests.scala:82)
     */
    def pcp: Unit = {
      val p1 = PolarVec(0, 0, 0)
      val p1a = p1.adjust
      val v1 = p1.toVec
      val pl2 = v1.toPolarVec
      println(s"pcp [${f(p1)} ${f(p1a)}] -> ${f(v1)} -> ${f(pl2)}")
    }

    def cpc: Unit = {
      val v0 = Vec(0.0, 0.0, 1.0)
      val vp = v0.toPolarVec
      val v1 = vp.toVec
      println(s"cpc ${f(v0)} -> ${f(vp)} -> ${f(v1)}")
    }

    def round: Unit = {
      def dround(x: Double) = {
        BigDecimal(x).setScale(13, BigDecimal.RoundingMode.HALF_UP).toDouble
      }

      for (scale <- 0 to 40) {
        val y = Random.nextDouble() * Random.nextInt(100)
        val x = dround(y)
        println("%22.18f".format(y))
        println("%22.18f".format(x))
      }
    }

    pcp

  }

  private def format(s: Star, pm: Vec): String =
    "  %7.2f %7.2f %7.2f | %7.2f %7.2f %7.2f ".format(
      s.pmra, s.pmdec, s.radialVelocity,
      pm.x, pm.y, pm.z)

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
    val ra = radToDeg(nearEcl.ra)
    val dec = radToDeg(nearEcl.dec)
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

package gaia

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.function.Consumer
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.util.zip.GZIPInputStream
import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.Random

object Tryout {

  import ImageUtil._
  import X3d._
  import Data._
  import Vector._
  import Cam._
  import Automove._


  def doit(args: List[String], workPath: Path): Unit = {
    viewpoint()
  }

  private def viewpoint(): Unit = {

    import Cam._

    val bc = Color.veryDarkGreen

    def someSpheres(color: Color): Seq[Shapable] = {

      def ran(range: Double): Double = {
        Random.nextDouble * range - range / 2.0
      }

      def ranRange(from: Double, to: Double): Double = {
        from + Random.nextDouble * (to - from)
      }

      val center = Vec(ran(30), ran(30), ran(30))
      (1 to 100)
        .map { _ =>
          val offset = Vec(ran(10), ran(10), ran(10))
          Shapable.Sphere(position = center.add(offset), color = color, radius = ranRange(0.1, 2))
        }
    }

    case class VQuality(
                         steps: Int,
                         geometry: (Int, Int) = (600, 420),
                         frameRates: Seq[Int] = Seq(1, 2, 4),
                         antiAlias: Int = 4,
                       )

    enum VideoQuality(val quality: VQuality) {

      case VGA extends VideoQuality(VQuality(60, (600, 420), Seq(1, 2, 4), 3))

      case SVGA extends VideoQuality(VQuality(120, (800, 600), Seq(2, 4, 8), 3))

      case HD extends VideoQuality(VQuality(240, (1200, 720), Seq(4, 8, 16), 2))

      case FullHD extends VideoQuality(VQuality(600, (1920, 1080), Seq(10, 20, 40), 2))

      case UltraHD extends VideoQuality(VQuality(1620, (2560, 1440), Seq(27, 54, 108), 1))

      case _4k extends VideoQuality(VQuality(1620, (3840, 2160), Seq(27, 54, 108), 1))

      case _4kwide extends VideoQuality(VQuality(1620, (4098, 2160), Seq(27, 54, 108), 1))

    }


    def mkVideo(
                 id: String,
                 shapables: Seq[Shapable],
                 fCams: Int => Seq[Camera],
                 videoQuality: VideoQuality) = {
      val outDir = Files.createTempDirectory(id)
      if Files.notExists(outDir) then Files.createDirectories(outDir)
      val videoOutDir = Main.workPath.resolve("videos")
      if Files.notExists(videoOutDir) then Files.createDirectories(videoOutDir)
      val numlen = 4
      val x3d0File = outDir.resolve(s"${id}.x3d")
      val quality: VQuality = videoQuality.quality
      val cams: Seq[Camera] = fCams(quality.steps)
      val xml = X3d.createXml(shapables, x3d0File.getFileName.toString, bc, cams)
      gaia.Util.writeString(x3d0File, xml)
      println(s"wrote to $x3d0File")
      val commands = cams
        .zipWithIndex
        .map { (c, i) =>
          val iFmt = "%0" + numlen + "d"
          val iStr = iFmt.format(i)

          val x3dFile = outDir.resolve(s"${id}_${iStr}.x3d")
          Files.copy(x3d0File, x3dFile)
          println(s"copied to $x3dFile")

          val geometryStr = s"${quality.geometry._1}x${quality.geometry._2}"
          val model = x3dFile.toAbsolutePath.toString
          val image = outDir.resolve(s"${id}_${iStr}.png").toAbsolutePath.toString
          Seq("view3dscene", model, "--anti-alias", quality.antiAlias.toString, "--viewpoint", i.toString,
            "--geometry", geometryStr, "--screenshot", "0", image)
        }

      println(commands.map(c => c.mkString(" ")).mkString("\n"))
      Util.runAllCommands(commands)
      println(s"finished ${commands.size} commands")
      val iFmtFf = "%0" + numlen + "d"
      val imgFile = outDir.resolve(s"${id}_$iFmtFf.png").toAbsolutePath.toString
      val videoId = videoQuality.toString
      val cmd = quality.frameRates.map { fr =>
        val outFile = videoOutDir.resolve(s"${id}_${videoId}_$fr.mp4").toAbsolutePath.toString
        Seq("ffmpeg", "-y", "-r", fr.toString, "-i", imgFile, outFile)
      }
      Util.runAllCommands(cmd)
      print("wrote video to " + videoOutDir + " - " + videoId)
    }

    VideoQuality.values.map { vq =>
      mkVideo("cam_tryout",
        Palette.p6c6.lazyColors.take(10).flatMap(c => someSpheres(c)),
        cameras(ra = 0, dec = 60, 100.0),
        vq
      )
    }
  }

  private def cam(): Unit = {

    val bc = Color.veryDarkRed

    val camShapes = (0 to(270, 90))
      .zip(X3d.Palette.p5c8.lazyColors)
      .flatMap { case (ra, c) =>
        cameras(ra = ra, dec = 60, 4.0)(20)
          .toSeq
          .flatMap { cam =>
            val dir = cam.dir.mul(0.05)
            Seq(
              X3d.Shapable.Cone(position = cam.pos, direction = dir, radius = 0.1, color = c),
              X3d.Shapable.Sphere(position = cam.pos, radius = 0.03, color = c),
            )
          }
      }
    val shapables = camShapes ++ ImageUtil.shapablesCoordinatesColored(len = 3, bgColor = bc)

    val file = Main.workPath.resolve("tryout_cam.x3d")
    val xml = X3d.createXml(shapables, file.getFileName.toString, bc)
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }

  private def cylinder(): Unit = {
    def fromDef = {
      val dirVecs = for (ra <- 0 to(350, 10); dec <- -70 to(70, 10)) yield {
        PolarVec(1, degToRad(ra), degToRad(dec)).toVec
      }
      dirVecs.map { dv =>
        val spd = StarPosDir(pos = Vec.zero, dir = dv)
        ImageUtil.shapeCylinder(Color.white, lengthFactor = 0.001)(starPosDir = spd)
      }
    }

    val shapables = fromDef
    val file = Main.workPath.resolve("tryout_cylinder.x3d")
    val xml = X3d.createXml(shapables, file.getFileName.toString, Color.gray(0.7))
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }


  private def displayCones(): Unit = {

    import ImageUtil._
    val bc = Color.veryDarkBlue

    val stars = TestStars.Cones.sparse
    val fromDef = stars.map(toStarPosDir).map(spd =>
      Shapable.Cylinder(position = spd.pos, direction = spd.dir)
    )

    val shapables = fromDef ++ shapablesCoordinatesColored(2, bc, showSign = true)
    val file = Main.workPath.resolve("tryout_cones.x3d")
    val xml = X3d.createXml(shapables, file.getFileName.toString, bc)
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
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

  def f(prefix: String, a: Double, b: Double, c: Double) = {
    def adj(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

    s"$prefix(%7.4f | %7.4f | %7.4f)".format(adj(a), adj(b), adj(c))
  }

  def f(v: Vec): String = f("C", v.x, v.y, v.z)

  def f(v: PolarVec): String = f("P", v.r, radToDeg(v.ra), radToDeg(v.dec))


  object TestStars {

    object Cones {
      private def moveStar(s: Star, o: Vec): Star = {
        val pv = PolarVec(1 / s.parallax, degToRad(s.ra), degToRad(s.dec))
        val cv = pv.toVec
        val mv = cv.add(o)
        val pv1 = mv.toPolarVec
        Star(parallax = 1 / pv1.r, ra = radToDeg(pv1.ra), dec = radToDeg(pv1.dec),
          pmra = s.pmra, pmdec = s.pmdec, radialVelocity = s.radialVelocity)
      }

      private def cone(offset: Vec,
                       coneSteps: Int = 10, decSteps: Int = 20, raSteps: Int = 45,
                       properMovement: Double = 0.005): Seq[Star] = {
        for (w <- 0 to(360 - coneSteps, coneSteps);
             dec <- (-90 + decSteps) to(90 - decSteps, decSteps);
             ra <- 0 to(360 - raSteps, raSteps)) yield {
          val x = properMovement * math.sin(degToRad(w))
          val y = properMovement * math.cos(degToRad(w))
          val cstar = Star(ra, dec, 1.0 / 600, x, y, 100)
          moveStar(cstar, offset)
        }
      }

      def rich: Seq[Star] = {
        cone(Vec.zero)
        ++ cone (Vec(0, 1500, 0)) ++ cone(Vec(0, -1500, 0))
        ++ cone (Vec(0, 0, 1500)) ++ cone(Vec(0, 0, -1500))
      }

      def sparse: Seq[Star] = {
        cone(Vec.zero, coneSteps = 20, properMovement = 0.01)
      }

      def spikes: Seq[Star] = {
        val decSteps = 10
        val raSteps = 10
        for (dec <- (-90 + decSteps) to(90 - decSteps, decSteps);
             ra <- 0 to(360 - raSteps, raSteps)) yield {
          val dist = 600 + Random.nextInt(200)
          val velo = 50 + Random.nextInt(100)
          Star(ra, dec, 1.0 / dist, 0, 0, velo)
        }
      }
    }

    def polarToCartTest(workPath: Path): Seq[Star] = {
      for (t <- 0 to(355, 5); d <- -80 to(30, 1)) yield {
        val dist = 20 + Random.nextDouble() * 0.1
        Star(ra = t, dec = d, parallax = 1.0 / dist, 0, 0, 0)
      }
    }

    def cartToPolarTest(workPath: Path): Seq[Star] = {
      val vecs = Seq(
        (-20 to 20).map(v => Vec(v, 0, 10)),
        (-20 to 20).map(v => Vec(v, 0, -10)),
        (-20 to 20).map(v => Vec(v, 10, 0)),
        (-20 to 20).map(v => Vec(v, -10, 0)),
        (-20 to 20).map(v => Vec(0, v, 10)),
        (-20 to 20).map(v => Vec(0, v, -10)),
        (-20 to 20).map(v => Vec(10, v, 0)),
        (-20 to 20).map(v => Vec(-10, v, 0)),
      ).flatten
      for (vc <- vecs) yield {
        //println(vc)
        val v = vc.toPolarVec
        Star(ra = radToDeg(v.ra), dec = radToDeg(v.dec), parallax = 1.0 / v.r, 0, 0, 0)
      }
    }

  }


}

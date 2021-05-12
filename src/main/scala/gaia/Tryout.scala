package gaia

import gaia.Gaia.VideoConfig

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.util.function.Consumer
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.language.{implicitConversions, postfixOps}
import scala.util.Random

object Tryout {

  import Cam._
  import Data._
  import ImageUtil._
  import Vector._
  import X3d._


  def doit(args: List[String], workPath: Path): Unit = {
    dirCol(workPath)
  }

  private def dirCol(workPath: Path): Unit = {
    val bc = Color.green

    def cos(dir: Vec) = {
      val w = Util.angle2DDeg(dir.x, dir.y)
      val min = 0.05
      val a = (1.0 - min) / 2.0
      val off = (2.0 + min) / 2.0
      val b = off + a * math.cos(degToRad(w))
      val c = Color.red.brightness(b)
      c
    }

    def cols(dir: Vec) = {
      val w = Util.angle2DDeg(dir.x, dir.y)
      val colors = Palette.p2c10.colors
      val d = colors.size.toDouble / 360
      val ci = math.floor(w * d).toInt
      colors(ci)
    }

    def coloredShape(dir: Vec): Shapable = {
      val dir1 = dir.copy(z = Random.nextDouble() * 4.0 - 2.0)
      val pos = Vec(x = Random.nextDouble() * 4.0 - 2.0, y = Random.nextDouble() * 4.0 - 2.0, z = 0.0)
      val c: Color = cols(dir)
      Shapable.Cone(position = pos, direction = dir1, color = c, radius = 0.05)
    }

    val shapables: Seq[Shapable] = (0 to(350, 10))
      .map(d => PolarVec(1, degToRad(d), 0).toVec)
      .map(coloredShape)

    val file = Gaia.workPath.resolve(s"tryout-dir-col.x3d")
    writeX3dFile1(bc, shapables, file)
  }

  private def allVids(): Unit = {
    Gaia.images
      .values
      .filter(_.videoConfig.exists(c => c.isInstanceOf[VideoConfig.Cams]))
      .map(_.id)
      .toSeq
      .sorted
      .foreach { id =>
        println(s"""sbt "run vid $id" """)
        println(s"""sbt "run still $id" """)
        println(s"""sbt "run cred $id" """)
      }
  }


  private def camRot(workPath: Path): Unit = {
    val bc = Color.veryDarkGreen
    //val shapables = ShapableFactory.sphereCoordinates(10)
    //val shapables = ImageFactory.sunms1(workPath, bc)
    //val shapables = ImageFactory.sund5(workPath, bc)
    val shapables = ImageFactory.sunms2(workPath, bc)
    val cams = cameras(100, 20, 15, eccentricity = 0.8, offset = Vec(5, 0, 0))(500)
    val duration = 60

    val xml: String = createCamAnimatedXml(shapables, cams, bc, duration, Rotation1(0, 0, 0))
    val file = Gaia.workPath.resolve(s"tryout_cam_rot.x3d")
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }


  private def x3dDir(workPath: Path): Unit = {
    val bc = Color.darkBlue

    def combi(): Seq[Shapable] = {

      def colVecs(vecs: Seq[Vec], color: Color = Color.orange): Seq[(Vec, Color)] = {
        def brightnes(n: Int): Seq[Double] = {
          val k = 0.8 / n
          (0 until n).map(x => 1.0 - k * x)
        }

        val bs = brightnes(vecs.size)
          .map(b => color.brightness(b))

        vecs.zip(bs)
      }

      def brightVecs(vecs: Seq[Vec]): Seq[(Vec, Double)] = {
        def brightnes(n: Int): Seq[Double] = {
          val k = 0.8 / n
          (0 until n).map(x => 1.0 - k * x)
        }

        vecs.zip(brightnes(vecs.size))
      }

      def degs: Seq[Int] = {

        @tailrec
        def fd(v: Int, li: List[Int]): List[Int] = {
          val d = 1 + Random.nextInt(5)
          if v + d >= 360 then li
          else fd(v + d, (v + d) :: li)
        }

        fd(0, List())

      }

      def d1: Double = -80 + Random.nextInt(160)

      val vecs0 = (0 to(359, 1))
        .map(a => degToRad(a))
        .map(a => PolarVec(1, a, degToRad(d1)).toVec)

      val vecs1 = degs
        .map(a => degToRad(a))
        .map(a => PolarVec(1, a, degToRad(d1)).toVec)

      val vecs2 = Seq(181, 200, 300)
        .map(a => degToRad(a))
        .map(a => PolarVec(1, a, degToRad(d1)).toVec)


      val vecs = brightVecs(vecs1)

      val old = vecs
        .map { (v, bright) =>
          Shapable.Cylinder(position = Vec.zero, direction = v, radius = 0.005, color = Color.orange.brightness(bright))
        }
      val news = vecs
        .flatMap { (v, bright) =>
          val rot = vecToRotation(v)
          println(s"$v $rot")
          val d0 = Vec(-rot.axis.z, 0, rot.axis.x)
          val dir1 = d0.norm.mul(0.5)
          val dir2 = rot.axis.norm.mul(0.5)
          Seq(
            Shapable.CylinderRot(rotation = rot, radius = 0.003, height = 1.1, color = Color.yellow.brightness(bright)),
            //Shapable.Cylinder(position = Vec.zero, direction = dir1, radius = 0.01, color = Color.white.brightness(bright)),
            //Shapable.Cylinder(position = Vec.zero, direction = dir2, radius = 0.01, color = Color.white.brightness(bright)),
          )
        }
      old ++ news
    }


    val shapables =
      combi()
        ++ shapablesCoordinatesColored(3, bc)
    val file = Gaia.workPath.resolve(s"tryout_x3dDir.x3d")
    writeX3dFile1(bc, shapables, file)
  }

  private def sunMove(): Unit = {
    val sunPos = toGalacticCoords(Vec(0, 0, 0))
    println(s"sunpos: $sunPos")
    println(s"sundir: $galacticSunDir")

    val sun = StarPosDir(pos = Vec.zero, dir = Vec.zero)
    val gsun = toStarPosDirGalactic(sun)

    println(gsun)
  }

  private def sunSpaceMotion(): Unit = {
    println("sun space motion")
    val id = "sun_space_motion"

    val bc = Color.black

    val sun = toStarPosDirGalactic(StarPosDir(pos = Vec(0, 0, 0), dir = Vec(1, 0, 0)))

    def galacticDist(s: Star) = toStarPosDirGalactic(s).pos.length

    val starsFiltered = readBasic
      .filter(galacticDist(_) < 50)
      .map(toStarPosDirGalactic)
      .toSeq

    val stars = Util.intervals(10, 0, pimul2)
      .zip(X3d.Palette.p3c11.lazyColors)
      .zip(LazyList.continually(starsFiltered))
      .flatMap { case (((from, to), c), sf) =>
        val in = sf.filter { spd =>
          val ra = {
            val w = math.asin(spd.pos.y / spd.pos.length) % pi
            if w < 0 then w + pimul2
            else w
          }
          ra >= from && ra < to
        }
        println(f"-- sector $from%.4f $to%.4f ${in.size}")
        Random.shuffle(in)
          .take(10)
          .zip(LazyList.continually(c))
      }

    println(s"filtered ${stars.size} stars")

    val shapesCircle = {
      (2 to(10, 2)).map { r =>
        Shapable.Circle(translation = Vec.zero,
          rotation = Vec(0, 0, 0),
          color = Color.gray(0.1), radius = r)
      }
    }
    val shapesCoord = ImageUtil.shapablesCoordinatesColored(10, bc)

    val shapableSun = Seq(
      Shapable.Sphere(position = sun.pos, color = Color.white, radius = 0.3)
    )

    val shapablesStars = stars
      .map { (star, c) =>
        val sg = toStarPosDirGalactic(star)
        Shapable.Sphere(position = sg.pos, color = c, radius = 0.1)
      }

    val shapables = shapesCircle ++ shapesCoord ++ shapableSun ++ shapablesStars

    val file = Gaia.workPath.resolve(s"tryout_$id.x3d")
    writeX3dFile1(bc, shapables, file)
  }

  private def sphereCoordinatesModel(): Unit = {
    val bc = Color.black
    val shapables = ShapableFactory.sphereCoordinates()
    val file = Gaia.workPath.resolve("tryout_sphere_coordinates.x3d")
    writeX3dFile1(bc, shapables, file)
  }

  private def cam(workPath: Path): Unit = {

    val bc = Color.veryDarkRed

    val camShapes = (0 to(180, 30))
      .zip(X3d.Palette.p5c8.lazyColors)
      .flatMap { (ra, c) =>
        cameras(raInDeg = ra, decInDeg = 60, 4.0)(20)
          .flatMap { cam =>
            val dir = cam.dir.mul(0.05)
            Seq(
              X3d.Shapable.Cone(position = cam.pos, direction = dir, radius = 0.1, color = c),
              X3d.Shapable.Sphere(position = cam.pos, radius = 0.03, color = c),
            )
          }
      }
    val shapables = camShapes ++ ImageUtil.shapablesCoordinatesColored(len = 3, bgColor = bc, showSign = true)

    val file = workPath.resolve("tryout_cam.x3d")
    writeX3dFile1(bc, shapables, file)
  }

  private def cylinder(): Unit = {
    def fromDef = {
      val dirVecs = for (ra <- 0 to(350, 10); dec <- -70 to(70, 10)) yield {
        PolarVec(1, degToRad(ra), degToRad(dec)).toVec
      }
      dirVecs.map { dv =>
        val spd = StarPosDir(pos = Vec.zero, dir = dv)
        Shapable.Cylinder(color = Color.white, position = spd.pos, direction = spd.dir.mul(0.001))
      }
    }

    val shapables = fromDef
    val file = Gaia.workPath.resolve("tryout_cylinder.x3d")
    val xml = X3d.createXml(shapables, file.getFileName.toString, Color.gray(0.7))
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }


  private def displayCones(): Unit = {

    import ImageUtil._
    val bc = Color.veryDarkBlue

    val stars = StarsFactory.Cones.sparse
    val fromDef = stars.map(toStarPosDir).map(spd =>
      Shapable.Cylinder(position = spd.pos, direction = spd.dir)
    )

    val shapables = fromDef ++ shapablesCoordinatesColored(2, bc, showSign = true)
    val file = Gaia.workPath.resolve("tryout_cones.x3d")
    writeX3dFile1(bc, shapables, file)
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
    val file = Gaia.workPath.resolve("tryout_directions.x3d")
    writeX3dFile1(bc, shapables, file)
  }

  private def vecConvert(): Unit = {

    def adj(): Unit = {
      def ad(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

      val y = -0.0
      val x = ad(y)
      val v1 = "%7.2f %7.2f".format(y, x)
      println(v1)
    }

    def norm(): Unit = {
      val v = PolarVec(1.0000, 4.1416, 3.1316)
      val vn = v.adjust
      println(s"norm $v -> $vn")
    }

    /*
    [info] - vector convert reconvert pcp PolarVec(6.0,2.0,-3.0) *** FAILED ***
    [info]   "... 6.0000 |  5.1416 | [-]0.1416)" was not equal to "... 6.0000 |  5.1416 | [ ]0.1416)" (Tests.scala:82)
     */
    def pcp(): Unit = {
      val p1 = PolarVec(0, 0, 0)
      val p1a = p1.adjust
      val v1 = p1.toVec
      val pl2 = v1.toPolarVec
      println(s"pcp [${f(p1)} ${f(p1a)}] -> ${f(v1)} -> ${f(pl2)}")
    }

    def cpc(): Unit = {
      val v0 = Vec(0.0, 0.0, 1.0)
      val vp = v0.toPolarVec
      val v1 = vp.toVec
      println(s"cpc ${f(v0)} -> ${f(vp)} -> ${f(v1)}")
    }

    def round(): Unit = {
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

    pcp()

  }

  private def format(s: Star, pm: Vec): String =
    "  %7.2f %7.2f %7.2f | %7.2f %7.2f %7.2f ".format(
      s.pmra, s.pmdec, s.radialVelocity,
      pm.x, pm.y, pm.z)

  private def execCmds(): Unit = {
    val cmds = Seq(
      Seq("wget", "https://google.com"),
      Seq("wget", "http://entelijan.net"),
      Seq("ping", "-c", "5", "entelijan.net"),
      Seq("ping", "-c", "2", "google.com"),
    )

    Util.runAllCommands(cmds)
  }

  private def amalyseStarsPerShell(): Seq[Unit] = {
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
      Some(star)
    }

    for ((id, min, max) <- shells) yield {
      val cnt = readBasic.flatMap(filterBasic(_)(min, max)).size
      println(s"$id - $cnt stars")
    }
  }

  private def analyseDataNegativeParallax(): Unit = {
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

  private def analyseDataBasicSize(): Unit = {
    val size = readBasic.size
    println(s"Size of basic is $size")
  }

  private def analyseDataBasicTop(): Unit = readBasic.take(20).foreach(println(_))

  private def analyseDataHeader(): Unit = {
    val fn = "GaiaSource_1000172165251650944_1000424567594791808.csv.gz"
    val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/$fn"
    println(s"downloading from $urlStr")
    val header = scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
      .getLines()
      .take(1)
      .toSeq
    header.head
      .split(",")
      .zipWithIndex
      .foreach { case (nam, i) => println("%4d - %s".format(i, nam)) }
  }

  def f(prefix: String, a: Double, b: Double, c: Double): String = {
    def adj(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

    s"$prefix(%7.4f | %7.4f | %7.4f)".format(adj(a), adj(b), adj(c))
  }

  def f(v: Vec): String = f("C", v.x, v.y, v.z)

  def f(v: PolarVec): String = f("P", v.r, radToDeg(v.ra), radToDeg(v.dec))


  object StarsFactory {

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
          ++ cone(Vec(0, 1500, 0)) ++ cone(Vec(0, -1500, 0))
          ++ cone(Vec(0, 0, 1500)) ++ cone(Vec(0, 0, -1500))
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

  object ShapableFactory {
    def sphereCoordinates(len: Int = 50): Seq[Shapable] = {
      val min = -len
      val max = len

      def sphere(value: Int, color: Color, f: Int => Vec): Shapable = {
        val c = if value == max then Color.white else color
        Shapable.Sphere(position = f(value), color = c, radius = len * 0.007)
      }

      Seq(
        (min to max).map(v => sphere(v, Color.red, v => Vec(v, 0, 0))),
        (min to max).map(v => sphere(v, Color.yellow, v => Vec(0, v, 0))),
        (min to max).map(v => sphere(v, Color.green, v => Vec(0, 0, v))),
      ).flatten
    }
  }

  private def writeX3dFile1(bc: Color, shapables: scala.Seq[Shapable], file: Path): Unit = {
    val xml = X3d.createXml(shapables, file.getFileName.toString, bc)
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }


}

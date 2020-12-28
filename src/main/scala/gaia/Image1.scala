package gaia

import com.sun.imageio.plugins.common.BogusColorSpace
import gaia.Data.Star
import gaia.Main.GaiaImage
import gaia.X3d.Shapable
import gaia.X3d.Shapable.Line

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Base64
import scala.util.Random


object Image1 {

  import X3d._

  case class ShellDef(
                       shellId: String,
                       startDist: Double,
                       endDist: Double,
                       starProb: Double,
                       starsToShapable: (stars: Iterable[Star]) => Iterable[Shapable],
                       transform: Star => Star = identity,
                     )

  case class StarPosDir(
                         dist: Double,
                         pos: Vec,
                         dir: Vec,
                       )

  object StarPosDir {

    def apply(star: Star): StarPosDir = {
      StarPosDir(1 / star.parallax, toVec(star), toDir(star))
    }

  }

  val galacicCenter = Util.toVec(266.25, -28.94, 8)

  def filterShells(ranges: Seq[(Double, Double)])(starPosDir: StarPosDir): Boolean = {
    def inRange(range: (Double, Double)): Boolean = {
      starPosDir.dist >= range._1 && starPosDir.dist < range._2
    }

    ranges.forall(inRange)
  }

  def dir01(id: String, workPath: Path): Unit = {
    val bgColor = gaiaImage(id).backColor
    val ranges = Seq((7.9, 8.1))

    def allShapables(bc: Color): Seq[Shapable] = {
      println(s"running $id")

      val colors = Palette.p2c10.colors
      val baseDirectionVec = Vec(1, 0, 1)
      val stars = basicStars(workPath)
        .map(StarPosDir.apply)
        .filter(filterShells(ranges)(_))
        .map { s =>
          val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(ci)
          X3d.Shapable.Line(start = s.pos, end = s.pos.add(s.dir.mul(0.005)), startColor = bc, endColor = c)
        }
      println(s"filtered ${stars.size} stars")
      stars
      ++ shapablesCoordinates(5, bgColor)
      ++ shapablesCoordinates(10, bgColor, offset = galacicCenter)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }

  def gc(id: String, workPath: Path): Unit = {
    val bgColor = gaiaImage(id).backColor
    val coordinatesColors = CoordinatesColors(Color.red, bgColor, Color.yellow, bgColor, Color.green, bgColor)
    val rotation = Vec(X3d.degToRad(-4), X3d.degToRad(96), X3d.degToRad(0))

    def allShapables(bc: Color): Seq[Shapable] = {
      Seq(Shapable.Circle(Vec(0, 0, 0), rotation = rotation, radius = 8))
      ++ shapablesCoordinates1(5, coordinatesColors)
      ++ shapablesCoordinates1(10, coordinatesColors, offset = galacicCenter)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }

  def nearSunVeloInner(id: String, workPath: Path): Unit = {
    val minDist = 0.0
    val maxDist = 0.04
    val colors = Palette.p1c10.colors
    val lengthFactor = 0.8
    val zoom = 100.0
    nearSunVelo(id, workPath, minDist, maxDist, colors, lengthFactor, zoom)
  }

  def nearSunVeloOuter(id: String, workPath: Path): Unit = {
    val minDist = 0.04
    val maxDist = 0.05
    val colors = Palette.p2c10.colors
    val lengthFactor = 1.0
    val zoom = 60.0
    nearSunVelo(id, workPath, minDist, maxDist, colors, lengthFactor, zoom)
  }

  private def nearSunVelo(id: String, workPath: Path, minDist: Double, maxDist: Double, colors: Seq[Color],
                          lengthFactor: Double, zoom: Double) = {
    val bgColor = gaiaImage(id).backColor
    val baseDirectionVec = Vec(1.0, 1.0, 0.0)

    def shapabels(stars: Iterable[Star]): Iterable[Shapable] = {
      stars
        .map(StarPosDir.apply)
        .flatMap { s =>
          val e = s.pos.add(s.dir.mul(0.00005 * lengthFactor))
          val a = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(a)
          Seq(Shapable.Line(start = e, end = s.pos, startColor = c, endColor = bgColor, zoom = zoom))
        }
    }

    def draw(bgColor: Color): Seq[Shapable] = {
      val stars = nearSunStars(workPath).filter { s =>
        val dist = 1 / s.parallax
        dist < maxDist && dist > minDist
      }
      println(s"There are ${stars.size} stars near the sun")
      shapabels(stars = stars).toSeq
      ++ shapablesCoordinates(maxDist * 1.2, bgColor, zoom = zoom)
    }

    createX3dFile(id, workPath, bgColor, draw _)
  }

  def nearSunDirections27pc(id: String, workPath: Path): Unit = {

    val radius = 0.00005
    val maxDist = 0.027
    val bgColor = gaiaImage(id).backColor

    def shapabels(radius: Double)(stars: Iterable[Star]): Iterable[Shapable] = {
      stars
        .map(StarPosDir.apply)
        .flatMap { s =>
          val br = 1 - (s.dist / (maxDist * 1.2))
          // println(s)
          val c = Color.orange.mul(br)
          Seq(Shapable.Cylinder(translation = s.pos, rotation = s.dir, color = c,
            radius = radius, height = radius * 100))
        }
    }

    def allShapables(bgColor: Color): Seq[Shapable] = {
      val stars = nearSunStars(workPath).filter(s => 1 / s.parallax < maxDist)
      println(s"There are ${stars.size} stars near the sun")
      shapabels(radius = radius)(stars = stars).toSeq
      ++ shapablesCoordinates(maxDist * 1.2, bgColor)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }


  def oneShellSpheres(id: String, workPath: Path): Unit = {
    val starsToShapable = shapabelsStarsToSperes(0.02, Color.gray(1.0))(_)
    val min = 7.0
    val max = 9.0
    val starProb = 0.1
    oneShell(id, workPath, min, max, starProb, starsToShapable)
  }

  def oneShellPoints(id: String, workPath: Path): Unit = {
    val starsToShapable = shapabelsStarsToPoints(Color.yellow)(_)
    val min = 7.0
    val max = 9.0
    val starProb = 1.0
    oneShell(id, workPath, min, max, starProb, starsToShapable)
  }

  def shellsSphere(id: String, workPath: Path): Unit = {

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("05", 5.0, 5.2, 200 / 1000.0, shapabelsStarsToSperes(0.04, Color.darkRed)(_)),
      ShellDef("08", 8.0, 8.2, 800 / 1000.0, shapabelsStarsToSperes(0.06, Color.orange)(_)),
      ShellDef("11", 11.0, 11.2, 1000 / 1000.0, shapabelsStarsToSperes(0.08, Color.yellow)(_)),
    )

    multiShells(id, workPath, shells)
  }

  def shellsPoints(id: String, workPath: Path): Unit = {

    def equalDist(dist: Double)(star: Star): Star = star.copy(parallax = 1.0 / dist)

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("a", 5.0, 6.0, 400 / 1000.0, shapabelsStarsToPoints(Color.yellow)(_), equalDist(5.5) _),
      ShellDef("b", 8.0, 9.0, 700 / 1000.0, shapabelsStarsToPoints(Color.orange)(_), equalDist(8) _),
      ShellDef("c", 11.0, 12.0, 1000 / 1000.0, shapabelsStarsToPoints(Color.darkRed)(_), equalDist(10.5) _),
    )

    multiShells(id, workPath, shells)
  }

  def sphere2(id: String, workPath: Path) = {
    println(s"Started image1 $id")
    val startProb = 0.0001
    val endProb = 0.1
    val shellCnt = 10
    val shellThikness = 0.2
    val startColor = Color.red
    val endColor = Color.yellow
    sphere(id, workPath, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sphere5(id: String, workPath: Path): Unit = {
    println(s"Started image1 $id")
    val shellCnt = 10
    val shellThikness = 0.5
    val startProb = 0.0001
    val endProb = 0.2
    val startColor = Color.blue
    val endColor = Color.green
    sphere(id, workPath, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sphere8(id: String, workPath: Path): Unit = {
    println(s"Started image1 $id")
    val shellCnt = 20
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.15
    val startColor = Color.darkRed
    val endColor = Color.orange
    sphere(id, workPath, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sphere16(id: String, workPath: Path): Unit = {
    println(s"Started image1 $id")
    val shellCnt = 40
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.6
    val startColor = Color.orange
    val endColor = Color.green
    sphere(id, workPath, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  private def sphere(id: String, workPath: Path, shellCnt: Int, shellThikness: Double,
                     startProb: Double, endProb: Double,
                     startColor: Color, endColor: Color) = {
    def draw(bgColor: Color): Seq[Shapable] = {
      val shells = (0 until shellCnt)
        .map(i => shellThikness * i)
        .map(s => (s, s + shellThikness))
      val colors = Util.colorTransition(startColor, endColor, shellCnt)
      val probs = Util.squaredValues(startProb, endProb, shellCnt)
      val cfgs = shells.zip(colors).zip(probs)
      cfgs.flatMap {
        case (((s, e), c), p) =>
          val stars = basicStars(workPath)
            .filter(filterShell(s, e, p)(_))
          println(f"filtered ${stars.size} stars for shell $s%.2f $p%.4f")
          shapabelsStarsToPoints(color = c)(stars = stars)
      }
      ++ shapablesCoordinates(5, bgColor)
    }

    createX3dFile(id, workPath, Color.black, draw _)
  }

  private def oneShell(id: String, workPath: Path, min: Double, max: Double, starProb: Double,
                       starsToShapable: Iterable[Star] => Iterable[Shapable]) = {
    def draw(bgColor: Color): Seq[Shapable] = {

      val stars = basicStars(workPath)
        .filter(filterShell(min, max, starProb)(_))
      println(s"using ${stars.size} stars for image1 $id")
      starsToShapable(stars).toSeq
      ++ shapablesCoordinates(5, bgColor)
      ++ shapablesCoordinates(10, bgColor, offset = galacicCenter)
    }

    createX3dFile(id, workPath, gaiaImage(id).backColor, draw _)
  }


  private def multiShells(id: String, workPath: Path, shells: Seq[ShellDef]) = {
    def draw(bgColor: Color): Seq[Shapable] = {
      val shapes = shells
        .flatMap { shellDef =>
          val stars = basicStars(workPath)
            .filter(filterShell(shellDef.startDist, shellDef.endDist, shellDef.starProb)(_))
            .map(shellDef.transform)
          println(s"Using ${stars.size} stars in shell ${shellDef.shellId}")
          shellDef.starsToShapable(stars)
        }
      shapes
      ++ shapablesCoordinates(5, bgColor)
      ++ shapablesCoordinates(10, bgColor, offset = galacicCenter)
    }

    val imgFile = x3dFilePath(workPath, id)
    X3d.drawTo(imgFile, draw, backColor = gaiaImage(id).backColor)
    println(s"Created image for $id at $imgFile")
  }

  def x3dFilePath(workPath: Path, id: String): Path = {
    val modelsPath = workPath.resolve("models")
    if (Files.notExists(modelsPath)) Files.createDirectories(modelsPath)
    val fnam = s"$id.x3d"
    modelsPath.resolve(fnam)
  }

  private def basicStars(workPath: Path): Seq[Star] = {

    def filter(star: Star): Boolean = {
      if (star.parallax <= 0) return false
      if (Random.nextDouble() >= 0.2) return false
      true
    }

    val cacheFile = createCacheFile("basic", workPath)
    starsFilteredAndCached(cacheFile, filter)
  }

  private def createCacheFile(id: String, workPath: Path): Path = {
    val cacheDir = workPath.resolve("cache")
    if (Files.notExists(cacheDir)) Files.createDirectories(cacheDir)
    cacheDir.resolve(s"cache_$id.csv")
  }

  private def nearSunStars(workPath: Path): Seq[Star] = {

    def filter(star: Star): Boolean = {
      if (star.parallax <= 0) return false
      val dist = 1.0 / star.parallax
      if (dist >= 0.1) return false
      true
    }

    val cacheFile = createCacheFile("near_sun", workPath)
    starsFilteredAndCached(cacheFile, filter)
  }

  private def filterShell(min: Double, max: Double, prob: Double)(star: Star): Boolean = {
    if (star.parallax <= 0) return false
    val dist = 1.0 / star.parallax
    if (dist < min || dist > max) return false
    if (Random.nextDouble() > prob) return false
    return true
  }

  private def shapabelsStarsToSperes(radius: Double, color: Color)(stars: Iterable[Star]): Iterable[Shapable] = {
    stars
      .map(toVec)
      .map(v => Shapable.Sphere(translation = v, color = color, radius = radius, solid = false))
  }

  private def shapabelsStarsToPoints(color: Color)(stars: Iterable[Star]): Iterable[Shapable] = {
    val vecs = stars.map(toVec)
    Seq(Shapable.PointSet(positions = vecs, color = color))
  }


  case class CoordinatesColors(
                                xStart: Color,
                                xEnd: Color,
                                yStart: Color,
                                yEnd: Color,
                                zStart: Color,
                                zEnd: Color
                              )

  private def shapablesCoordinates(len: Double, bgColor: Color, offset: Vec = Vec.zero, zoom: Double = 1.0): Seq[Shapable] = {
    val ccs = CoordinatesColors(
      Color.gray(0.9), bgColor, Color.gray(0.9), bgColor, Color.gray(0.9), bgColor)
    shapablesCoordinates1(len, ccs, offset, zoom)
  }


  private def shapablesCoordinates1(len: Double, coordinatesColors: CoordinatesColors, offset: Vec = Vec.zero, zoom: Double = 1.0): Seq[Shapable] = {
    def ends = Seq(
      (Vec(1, 0, 0).mul(len).add(offset), Vec(-1, 0, 0).mul(len).add(offset), coordinatesColors.xStart, coordinatesColors.xEnd),
      (Vec(0, 1, 0).mul(len).add(offset), Vec(0, -1, 0).mul(len).add(offset), coordinatesColors.yStart, coordinatesColors.yEnd),
      (Vec(0, 0, 1).mul(len).add(offset), Vec(0, 0, -1).mul(len).add(offset), coordinatesColors.zStart, coordinatesColors.zEnd),
    )

    def coord(e1: Vec, e2: Vec, startColor: Color, endColor: Color): Seq[Line] = {
      Seq(
        Shapable.Line(start = offset, end = e1, startColor = startColor, endColor = endColor, zoom),
        Shapable.Line(start = offset, end = e2, startColor = startColor, endColor = endColor, zoom),
      )
    }

    ends.flatMap {
      coord
    }
  }

  private def toVec(star: Star): Vec = {
    val dist = 1 / star.parallax
    val ra = star.ra
    val dec = star.dec
    Util.toVec(ra, dec, dist)
  }

  private def toDir(star: Star): Vec = {

    // in mas milli arc seconds per year
    val ra = star.pmra

    // in mas milli arc seconds per year
    val dec = star.pmdec
    val dist = 1 / star.parallax

    // in km / second
    val z = star.radialVelocity

    //  1 Parsec = 3.08567758e16 Meter 
    val parsecToMeter = 3.08567758e16

    // 1 Year = 3.154 Seconds
    val yearToSeconds = 3.154e7
    val k0 = math.Pi * 1e-6 / (3 * 180)
    val x = dist * k0 * ra * parsecToMeter / yearToSeconds
    val y = dist * k0 * dec * parsecToMeter / yearToSeconds
    Vec(x, y, z)
  }

  private def starsFilteredAndCached(cache: Path, filterStar: Star => Boolean): Seq[Star] = {

    def fromCsv(): Seq[Star] = {
      def convert(a: Array[String]): Star = {
        Star(
          ra = a(0).toDouble,
          dec = a(1).toDouble,
          parallax = a(2).toDouble,
          pmra = a(3).toDouble,
          pmdec = a(4).toDouble,
          radialVelocity = a(5).toDouble,
        )
      }

      Util.fromCsv(convert, cache).toSeq
    }

    def toCsv(starts: Seq[Star]): Unit = {

      def convert(s: Star): Iterable[String] = {
        Seq(
          s.ra.toString,
          s.dec.toString,
          s.parallax.toString,
          s.pmra.toString,
          s.pmdec.toString,
          s.radialVelocity.toString,
        )
      }

      Util.toCsv(starts, convert, cache)
    }

    if (Files.exists(cache)) {
      val stars = fromCsv()
      println(s"filtered (cached) basic to ${stars.size} stars")
      stars
    }
    else {
      println(s"creating data from base and store in $cache")
      val stars = Data.readBasic
        .filter(filterStar)
        .toSeq
      toCsv(stars)
      println(s"filtered basic to ${stars.size} stars")
      stars
    }
  }

  private def createX3dFile(id: String, workPath: Path, bgColor: Color, createShapables: Color => Seq[Shapable]) = {
    val file = x3dFilePath(workPath, id)
    X3d.drawTo(file, createShapables, backColor = bgColor)
    println(s"Created image for $id at ${file.toAbsolutePath}")
  }

  def gaiaImage(id: String): GaiaImage = {
    Main.images(id)
  }
}

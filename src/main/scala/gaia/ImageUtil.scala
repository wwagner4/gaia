package gaia


import java.nio.file.{Files, Path}
import scala.util.Random

object ImageUtil {

  import Data.Star
  import Main._
  import X3d._

  case class StarPosDir(
                         pos: Vec,
                         dir: Vec,
                       )

  case class CoordinatesColors(
                                xStart: Color,
                                xEnd: Color,
                                yStart: Color,
                                yEnd: Color,
                                zStart: Color,
                                zEnd: Color
                              )


  case class ShellDef(
                       shellId: String,
                       startDist: Double,
                       endDist: Double,
                       starProb: Double,
                       starsToShapable: (stars: Iterable[Star]) => Iterable[Shapable],
                       transform: Star => Star = identity,
                     )

  val galacicCenter = X3d.toVec(266.25, -28.94, 8)

  def nearSunStars(workPath: Path): Seq[Star] = {

    def filter(star: Star): Boolean = {
      if (star.parallax <= 0) return false
      val dist = 1.0 / star.parallax
      if (dist >= 0.1) return false
      true
    }

    val cacheFile = createCacheFile("near_sun", workPath)
    starsFilteredAndCached(cacheFile, filter)
  }

  def basicStars(workPath: Path): Seq[Star] = {

    def filter(star: Star): Boolean = {
      if (star.parallax <= 0) return false
      if (Random.nextDouble() >= 0.2) return false
      true
    }

    val cacheFile = createCacheFile("basic", workPath)
    starsFilteredAndCached(cacheFile, filter)
  }

  def testStars(workPath: Path): Seq[Star] = {
    for (w <- 0 to (355, 5); dec <- -80 to(80, 20); ra <- 0 to(340, 20)) yield {
      val x = 0.01 * math.sin(X3d.degToRad(w))
      val y = 0.01 * math.cos(X3d.degToRad(w))
      Star(ra, dec, 1.0 / 600, x, y, 60)
    }
  }

  private def createCacheFile(id: String, workPath: Path): Path = {
    val cacheDir = workPath.resolve("cache")
    if (Files.notExists(cacheDir)) Files.createDirectories(cacheDir)
    cacheDir.resolve(s"cache_$id.csv")
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


  private def starToVec(star: Star): Vec = {
    val dist = 1 / star.parallax
    val ra = star.ra
    val dec = star.dec
    X3d.toVec(ra, dec, dist)
  }

  // constants for 'toDir'
  private val parsecToMeter = 3.08567758e16
  private val yearToSeconds = 3.154e7
  private val k0 = math.Pi * 1e-6 / (3 * 180)
  private val k1 = k0 * parsecToMeter / yearToSeconds

  private def toDir(star: Star): Vec = {
    val dist = 1 / star.parallax
    val z = dist * star.pmra * k1
    val y = dist * star.pmdec * k1
    val x = star.radialVelocity
    val sp = Vec(x, y, z).toPolarVec
    val sp1 = if x > 0 then
      sp.copy(
        dec = sp.dec + X3d.degToRad(star.ra),
        ra = sp.ra - X3d.degToRad(180 - star.dec),
        r= if x > 0 then sp.r * -1 else sp.r)
    else
      sp.copy(
        dec = sp.dec + X3d.degToRad(star.ra),
        ra = sp.ra - X3d.degToRad(star.dec),
        r= if x > 0 then sp.r * -1 else sp.r)
    sp1.toVec
  }

  def writeModelToFile[T](converter: Star => T, reader: Path => Iterable[Star])(fcreateShapables: (Iterable[T], X3d.Color) => Seq[Shapable])(id: String, workPath: Path): Unit = {
    val gaiaImage = Main.images(id)
    val bgColor = gaiaImage.backColor
    val stars = reader(workPath)
    val starsConverted = stars.map(converter)
    val shapables = fcreateShapables(starsConverted, bgColor)
    val file = {
      val modelsPath = workPath.resolve("models")
      if (Files.notExists(modelsPath)) Files.createDirectories(modelsPath)
      val fnam = s"$id.x3d"
      modelsPath.resolve(fnam)
    }

    val xml = X3d.createXml(shapables, file.getFileName.toString, bgColor)
    gaia.Util.writeString(file, xml)
    println(s"Created image for $id at ${file.toAbsolutePath}")
  }

  def shapablesCoordinatesGray(len: Double, bgColor: Color, offset: Vec = Vec.zero): Seq[Shapable] = {
    shapablesCoordinatesOneColor(len, Color.gray(0.9), bgColor, offset)
  }

  def shapablesCoordinatesOneColor(len: Double, color: Color, bgColor: Color, offset: Vec = Vec.zero): Seq[Shapable] = {
    val ccs = CoordinatesColors(color, bgColor, color, bgColor, color, bgColor)
    shapablesCoordinates(len, ccs, offset)
  }

  def shapablesCoordinatesColored(len: Double, bgColor: Color, offset: Vec = Vec.zero): Seq[Shapable] = {
    val ccs = CoordinatesColors(
      Color.red, bgColor,
      Color.yellow, bgColor,
      Color.green, bgColor)
    shapablesCoordinates(len, ccs, offset)
  }

  private def shapablesCoordinates(len: Double, coordinatesColors: CoordinatesColors, offset: Vec = Vec.zero): Seq[Shapable] = {
    def ends = Seq(
      (Vec(1, 0, 0).mul(len).add(offset), Vec(-1, 0, 0).mul(len).add(offset), coordinatesColors.xStart, coordinatesColors.xEnd),
      (Vec(0, 1, 0).mul(len).add(offset), Vec(0, -1, 0).mul(len).add(offset), coordinatesColors.yStart, coordinatesColors.yEnd),
      (Vec(0, 0, 1).mul(len).add(offset), Vec(0, 0, -1).mul(len).add(offset), coordinatesColors.zStart, coordinatesColors.zEnd),
    )

    def coord(e1: Vec, e2: Vec, startColor: Color, endColor: Color): Seq[Shapable.Line] = {
      Seq(
        Shapable.Line(start = offset, end = e1, startColor = startColor, endColor = endColor),
        Shapable.Line(start = offset, end = e2, startColor = startColor, endColor = endColor),
      )
    }

    ends.flatMap {
      coord
    }
  }

  def sphere(stars1: Iterable[Star], bc: Color, shellCnt: Int, shellThikness: Double,
             startProb: Double, endProb: Double,
             startColor: Color, endColor: Color) = {
    val shells = (0 until shellCnt)
      .map(i => shellThikness * i)
      .map(s => (s, s + shellThikness))
    val colors = Util.colorTransition(startColor, endColor, shellCnt)
    val probs = Util.squaredValues(startProb, endProb, shellCnt)
    val cfgs = shells.zip(colors).zip(probs)
    cfgs.flatMap {
      case (((s, e), c), p) =>
        val stars = stars1
          .filter(filterShell(s, e, p)(_))
        println(f"filtered ${stars.size} stars for shell $s%.2f $p%.4f")
        shapabelsStarsToPoints(color = c)(stars = stars)
    }
    ++ shapablesCoordinatesGray(5, bc)
  }

  def oneShell(stars1: Iterable[Star], bc: Color, min: Double, max: Double, starProb: Double,
               starsToShapable: Iterable[Star] => Iterable[Shapable]): Seq[Shapable] = {

    val stars = stars1
      .filter(filterShell(min, max, starProb)(_))

    starsToShapable(stars).toSeq ++ shapablesCoordinatesGray(5, bc) ++ shapablesCoordinatesGray(10, bc, offset = galacicCenter)
  }

  def multiShells(stars1: Iterable[Star], bc: Color, shells: Seq[ShellDef]): Seq[Shapable] = {
    val shapes = shells
      .flatMap { shellDef =>
        val stars = stars1
          .filter(filterShell(shellDef.startDist, shellDef.endDist, shellDef.starProb)(_))
          .map(shellDef.transform)
        println(s"Using ${stars.size} stars in shell ${shellDef.shellId}")
        shellDef.starsToShapable(stars)
      }
    shapes
    ++ shapablesCoordinatesGray(5, bc)
    ++ shapablesCoordinatesGray(10, bc, offset = galacicCenter)
  }

  def filterShell(min: Double, max: Double, prob: Double)(star: Star): Boolean = {
    if (star.parallax <= 0) return false
    val dist = 1.0 / star.parallax
    if (dist < min || dist > max) return false
    if (Random.nextDouble() > prob) return false
    return true
  }

  def shapabelsStarsToSpheres(radius: Double, color: Color)(stars: Iterable[Star]): Iterable[Shapable] = {
    stars
      .map(starToVec)
      .map(v => Shapable.Sphere(position = v, color = color, radius = radius, solid = false))
  }

  def shapabelsStarsToPoints(color: Color)(stars: Iterable[Star]): Iterable[Shapable] = {
    val vecs = stars.map(starToVec)
    Seq(Shapable.PointSet(positions = vecs, color = color))
  }

  def nearSunVelo(stars: Iterable[Star], bgColor: Color, minDist: Double, maxDist: Double, colors: Seq[Color], lengthFactor: Double): Seq[Shapable] = {
    val baseDirectionVec = Vec(1.0, 1.0, 0.0)

    val starsFiltered = stars.filter { s =>
      val dist = 1 / s.parallax
      dist < maxDist && dist > minDist
    }

    println(s"There are ${starsFiltered.size} stars near the sun")

    val starsShapable = starsFiltered.toSeq
      .map(toStarPosDir)
      .flatMap { s =>
        val e = s.pos.add(s.dir.mul(0.00005 * lengthFactor))
        val a = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
        val c = colors(a)
        Seq(Shapable.Line(start = e, end = s.pos, startColor = c, endColor = bgColor))
      }

    starsShapable ++ shapablesCoordinatesGray(maxDist * 1.2, bgColor)
  }

  def filterShells(ranges: Seq[(Double, Double)])(starPosDir: StarPosDir): Boolean = {
    def inRange(range: (Double, Double)): Boolean = {
      val dist = starPosDir.pos.length
      dist >= range._1 && dist < range._2
    }

    ranges.exists(inRange)
  }

  def toStarPosDirGalactic(star: Star): StarPosDir = {
    val spd = toStarPosDir(star)
    val gcoord = spd.pos.sub(galacicCenter)
    val gpos = toGalacticCoords(gcoord)
    spd.copy(pos = gpos)
  }

  def toStarPosDir(star: Star): StarPosDir =
    StarPosDir(starToVec(star), toDir(star))

  def toGalacticCoords(pos: Vec): Vec =
    pos
      .rotx(X3d.degToRad(-27.13))
      .roty(X3d.degToRad(-28.94))

  def inCube(cubeSize: Int, cubeCount: Int)(pos: Vec, i: Int, j: Int, k: Int): Boolean = {
    val ix = math.floor(pos.x * cubeCount / cubeSize).toInt
    val iy = math.floor(pos.y * cubeCount / cubeSize).toInt
    val iz = math.floor(pos.z * cubeCount / cubeSize).toInt
    ix == i && iy == j && iz == k
  }

}

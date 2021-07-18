package gaia


import upickle.default.{macroRW, read, write, ReadWriter => RW}

import java.nio.file.Files._
import java.nio.file.{Files, Path}
import scala.util.Random

object ImageUtil {

  import Data.Star
  import Gaia._
  import Vector._
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

  lazy val galacicCenter = PolarVec(r = 8, ra = degToRad(266.25), dec = degToRad(-28.94)).toVec

  lazy val galacticSunDir = {
    val sunPos = toGalacticCoords(Vec(0, 0, 0))
    Vec(sunPos.y, sunPos.x, 0).norm.mul(235)
  }


  object StarCollections {
    def basicStars(workPath: Path): Seq[Star] = {

      def filter(star: Star): Boolean = {
        if (star.parallax <= 0) return false
        if (Random.nextDouble() >= 0.2) return false
        true
      }

      val cacheFile = createCacheFile("basic", workPath)
      starsFilteredAndCached(cacheFile, filter, workPath)
    }

    def nearSunStars(workPath: Path): Seq[Star] = {

      def filter(star: Star): Boolean = {
        if (star.parallax <= 0) return false
        val dist = 1.0 / star.parallax
        if (dist >= 0.1) return false
        true
      }

      val cacheFile = createCacheFile("near_sun", workPath)
      starsFilteredAndCached(cacheFile, filter, workPath)
    }


  }

  private def createCacheFile(id: String, workPath: Path): Path = {
    val cacheDir = workPath.resolve("cache")
    if (notExists(cacheDir)) createDirectories(cacheDir)
    cacheDir.resolve(s"cache_$id.csv")
  }

  private def starsFilteredAndCached(cache: Path, filterStar: Star => Boolean, workPath: Path): Seq[Star] = {

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

    if (exists(cache)) {
      val stars = fromCsv()
      println(s"filtered (cached) basic to ${stars.size} stars")
      stars
    }
    else {
      println(s"creating data from base and store in $cache")
      val stars = Data.readBasic(workPath)
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
    PolarVec(dist, degToRad(ra), degToRad(dec)).toVec
  }

  // constants for 'properMotionToSpaceMotion'
  private lazy val parsecToMeter = 3.08567758e16
  private lazy val yearToSeconds = 3.154e7
  private lazy val k0 = math.Pi * 1e-6 / (3 * 180)
  private lazy val k1 = k0 * parsecToMeter / yearToSeconds

  def properMotionToSpaceMotion(star: Star): Vec = {
    val dist = 1 / star.parallax
    val y = dist * star.pmra * k1
    val z = dist * star.pmdec * k1
    val x = star.radialVelocity
    Vec(x, y, z)
  }

  def spaceMotionToGalacticMotion(star: Data.Star, spaceMotion: Vec): Vec = {
    val rra = degToRad(star.ra)
    val rdec = degToRad(star.dec)
    spaceMotion
      .roty(-rdec)
      .rotz(rra)
  }

  private def toDir(star: Star): Vec = {
    val sm = properMotionToSpaceMotion(star)
    spaceMotionToGalacticMotion(star, sm)
  }

  def shapablesCoordinatesGray(len: Double, bgColor: Color, offset: Vec = Vec.zero, brightness: Double = 0.9): Seq[Shapable] = {
    shapablesCoordinatesOneColor(len, Color.gray(brightness), bgColor, offset)
  }

  def shapablesCoordinatesOneColor(len: Double, color: Color, bgColor: Color, offset: Vec = Vec.zero): Seq[Shapable] = {
    val ccs = CoordinatesColors(color, bgColor, color, bgColor, color, bgColor)
    shapablesCoordinates(len, ccs, offset, false)
  }

  def shapablesCoordinatesColored(len: Double, bgColor: Color, offset: Vec = Vec.zero, showSign: Boolean = false): Seq[Shapable] = {
    val ccs = CoordinatesColors(
      Color.red, bgColor,
      Color.yellow, bgColor,
      Color.green, bgColor)
    shapablesCoordinates(len, ccs, offset, showSign)
  }

  private def shapablesCoordinates(len: Double, coordinatesColors: CoordinatesColors, offset: Vec = Vec.zero, showSign: Boolean): Seq[Shapable] = {
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

    val signShapes = Seq(
      Shapable.Sphere(position = Vec(len, 0, 0), radius = len / 50.0, color = Color.white),
      Shapable.Sphere(position = Vec(0, len, 0), radius = len / 50.0, color = Color.white),
      Shapable.Sphere(position = Vec(0, 0, len), radius = len / 50.0, color = Color.white))

    val shapes = ends.flatMap {
      coord
    }
    if showSign then shapes ++ signShapes
    else shapes
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

  def nearSunVelo(stars: Iterable[Star], bgColor: Color, minDist: Double, maxDist: Double, colors: Seq[Color],
                  lengthFactor: Double, radius: Double): Seq[Shapable] = {
    val baseDirectionVec = Vec(1.0, 1.0, 0.0)

    val starsFiltered = stars.filter { s =>
      val dist = 1 / s.parallax
      dist < maxDist && dist > minDist
    }

    println(s"There are ${starsFiltered.size} stars near the sun")

    val starsShapable = starsFiltered.toSeq
      .map(toStarPosDir)
      .map { s =>
        val a = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
        val c = colors(a)
        Shapable.Cone(color = c, position = s.pos, radius = radius, direction = s.dir.mul(lengthFactor))
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
    toStarPosDirGalactic(spd)
  }

  def toStarPosDirGalactic(star: StarPosDir): StarPosDir = {
    val gpos = toGalacticCoords(star.pos)
    star.copy(pos = gpos, dir = star.dir.add(galacticSunDir))
  }

  def toStarPosDir(star: Star): StarPosDir =
    StarPosDir(starToVec(star), toDir(star))

  def toGalacticCoords(pos: Vec): Vec = {
    val p1 = pos.sub(galacicCenter)
    val rotated = p1
      .rotx(degToRad(-27.13))
      .roty(degToRad(-28.94))
    Vec(rotated.z, rotated.y, rotated.x)
  }

  trait CubeSplit {
    def cubeSize: Int
    def cubeCount: Int
  }

  enum CubeSplits(val cubeSize: Int, val cubeCount: Int) extends CubeSplit {
    // TODO Check cube size and cube count. Currently much too large
    case rough extends CubeSplits(16,16)
    case medium extends CubeSplits(8,32)
    case fine extends CubeSplits(4,64)
  }

  case class Cube(i: Int, j: Int, k: Int)

  def probsFromResource(cubeSplit: CubeSplits, minCount: Int = 10): Seq[(Cube, Double)] = {
    def resName(cubeSplit: CubeSplits, minCount: Int): String = {
      val cubeSplitCode = cubeSplit match {
        case CubeSplits.fine => "f"
        case CubeSplits.medium => "m"
        case CubeSplits.rough => "r"
      }
      val minCountsStr = "%03d".format(minCount)
      s"dens/dens-$cubeSplitCode$minCountsStr.pkl"
    }

    val rn = resName(cubeSplit, minCount)
    read[Seq[(Cube, Double)]](Util.fromResource(rn))
  }

  object Cube {
    implicit val rw2: RW[Cube] = macroRW
  }

  def positionToCube(cubeSplit: CubeSplit)(position: Vec): Option[Cube] = {
    val v2i = Util.valueToIndex(cubeSplit.cubeSize, cubeSplit.cubeCount)
    val i = v2i(position.x)
    val j = v2i(position.y)
    val k = v2i(position.z)
    if i.isDefined && j.isDefined && k.isDefined
    then Some(Cube(i.get, j.get, k.get))
    else None
  }

  def cubeIterator(cubeCount: Int): Seq[Cube] = {
    for (i <- -cubeCount until cubeCount;
         j <- -cubeCount until cubeCount;
         k <- -cubeCount until cubeCount) yield Cube(i, j, k)
  }

  /**
   * Descide weather a position is inside a cube or not
   */
  def inCube(cubeSplit: CubeSplit)(pos: Vec, cube: Cube): Boolean = {
    val c1 = positionToCube(cubeSplit)(pos)
    c1 == cube
  }

  def shapeLine(backColor: Color, endColor: Color)(starPosDir: StarPosDir): Shapable = {
    val a = starPosDir.pos
    val b = starPosDir.pos.add(starPosDir.dir)
    Shapable.Line(start = a, end = b, startColor = backColor, endColor = endColor)
  }

  def circleShapes(distKpc: Double, cnt: Int, color: Color = Color.gray(0.7)): Seq[Shapable] = {

    def dists: Seq[Double] = {
      val diff = distKpc / cnt
      val eps = 0.000001

      def distsRecursive(currentValue: Double, result: List[Double]): List[Double] = {
        if currentValue > distKpc + eps then result
        else distsRecursive(currentValue + diff, currentValue :: result)
      }

      distsRecursive(diff, List())
    }

    dists.map(r => Shapable.Circle(translation = Vec.zero,
      rotation = Vec(0, 0, 0),
      color = color, radius = r))
  }


  def colorFromDirection(a: Double, b: Double, palette: Palette = PaletteBrew.p2c10) = {
    val w = Util.angle2DDeg(a, b)
    val colors = palette.colors
    val d = colors.size.toDouble / 360
    val ci = math.floor(w * d).toInt
    colors(ci)
  }


}

package gaia

import com.sun.imageio.plugins.common.BogusColorSpace

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Base64
import scala.util.Random


object Image1 {

  import X3d._
  import ImageUtil._
  import Data._
  import Vector._

  def sund4(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val ranges = Seq((7.9, 8.1))
    val colors = Palette.p2c10.colors
    val baseDirectionVec = Vec(1, 0, 1)
    val starShapes =
      stars.map(toStarPosDir)
        .filter(filterShells(ranges)(_))
        .map { s =>
          val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(ci)
          shapeCone(color = c, lengthFactor = 0.005, geo = Geo.Absolute(0.01))(s)
        }
    println(s"created ${starShapes.size} shapes")
    starShapes.toSeq
    ++ shapablesCoordinatesGray(5, bc)
    ++ shapablesCoordinatesGray(10, bc, offset = ImageUtil.galacicCenter)
  }

  def sund5(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val ranges = Seq(
      (9.6, 10.0),
      (7.8, 8.0),
      (5.95, 6.0),
    )

    def adjust(star: StarPosDir): StarPosDir = {
      val dist = star.pos.length
      val dist1 = math.ceil(dist)
      val pos1 = star.pos.norm.mul(dist1)
      val dir1 = star.dir.norm.mul(-1.0)
      star.copy(pos = pos1, dir = dir1)
    }

    val colors = Palette.p5c8.colors
    val baseDirectionVec = Vec(1, -1, 1)
    val starShapables = stars.map(toStarPosDir)
      .filter(filterShells(ranges)(_))
      .map { s =>
        val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
        val c = colors(ci)
        shapeCone(color = c, lengthFactor=0.003, geo = Geo.Absolute(0.03))(s)
      }
    println(s"created ${starShapables.size} shapes")
    starShapables.toSeq
    ++ shapablesCoordinatesGray(5, bc)
    ++ shapablesCoordinatesGray(10, bc, offset = ImageUtil.galacicCenter)
  }

  def sund6(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val epsBase = 0.4
    val epsAdj = Seq(
      8 -> 1.0 / 13,
      9 -> 1.0 / 9,
      10 -> 1.0 / 7,
      11 -> 1.0 / 5,
      12 -> 1.0 / 4,
      13 -> 1.0 / 3,
      14 -> 1.0 / 2,
      15 -> 1.0 / 2,
      16 -> 1.0 / 2,
    ).toMap

    def eps(dist: Int): Double = {
      epsAdj.get(dist).map(adj => adj * epsBase).getOrElse(epsBase)
    }

    val ranges = (5 to (20, 5))
      .toSeq
      .map(v => (v.toDouble - eps(v), v.toDouble))

    def adjust(star: StarPosDir): StarPosDir = {
      val dist = star.pos.length
      val dist1 = math.ceil(dist)
      val pos1 = star.pos.norm.mul(dist1)
      star.copy(pos = pos1)
    }

    val colors = Palette.p5c8.colors
    val baseDirectionVec = Vec(1, -1, 1)
    val starss = stars.map(toStarPosDirGalactic)
      .filter(_ => Random.nextDouble() < 0.3)
      .filter(filterShells(ranges)(_))
      .map(adjust)
      .map { s =>
        val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
        val c: Color = colors(ci)
        X3d.Shapable.Sphere(position = s.pos, color = c, radius = 0.01)
      }
    println(s"filtered ${starss.size} stars")
    starss.toSeq
    ++ shapablesCoordinatesGray(10, bc, brightness = 0.2)
  }
  
  def sund1(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.nearSunStars(workPath)
    val minDist = 0.0
    val maxDist = 0.03
    val colors = Palette.p1c10.colors
    val lengthFactor = 0.00003
    val geo = Geo.Absolute(0.0002)
    nearSunVelo(stars, bc, minDist, maxDist, colors, lengthFactor, geo)
  }

  def sund2(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.nearSunStars(workPath)
    val minDist = 0.03
    val maxDist = 0.04
    val colors = Palette.p1c10.colors
    val lengthFactor = 2.0
    val baseDirectionVec = Vec(1.0, 1.0, 0.0)

    def shapabels(stars: Iterable[Star]): Iterable[Shapable] = {
      stars
        .map(s => s.copy(pmra = 0.0, pmdec = 0.0))
        .map(toStarPosDir)
        .map { s =>
          val e = s.pos.add(s.dir.mul(0.00005 * lengthFactor))
          val a = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(a)
          Shapable.Line(start = s.pos, end = e, startColor = c, endColor = bc)
        }
    }

    val starsFiltered = stars.filter { s =>
      val dist = 1 / s.parallax
      dist < maxDist && dist > minDist
    }
    println(s"There are ${starsFiltered.size} stars near the sun")
    shapabels(stars = starsFiltered).toSeq
    ++ shapablesCoordinatesColored(maxDist * 1.2, bc)
  }

  def sund3(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.nearSunStars(workPath)

    val minDist = 0.04
    val maxDist = 0.05
    val colors = Palette.p2c10.colors
    val lengthFactor = 0.00008
    val geo = Geo.Absolute(0.00006)
    nearSunVelo(stars, bc, minDist, maxDist, colors, lengthFactor, geo = geo)
  }

  def sund27(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.nearSunStars(workPath)

    val radius = 0.00005
    val maxDist = 0.027

    def shapabels(radius: Double)(stars: Iterable[Star]): Iterable[Shapable] = {
      stars
        .map(toStarPosDir)
        .map { s =>
          val br = 1 - (s.pos.length / (maxDist * 1.2))
          val c = Color.orange.mul(br)
          shapeCylinder(c, lengthFactor = 0.00005)(s)
        }
    }

    val sl = stars.filter(s => 1 / s.parallax < maxDist)
    println(s"There are ${sl.size} stars near the sun")
    shapabels(radius = radius)(stars = sl).toSeq
    ++ shapablesCoordinatesGray(maxDist * 1.2, bc)
  }


  def sunos1(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars1 = StarCollections.basicStars(workPath)
    val starsToShapable = shapabelsStarsToSpheres(0.02, Color.gray(1.0))(_)
    val min = 7.0
    val max = 9.0
    val starProb = 0.1
    oneShell(stars1, bc, min, max, starProb, starsToShapable)
  }

  def sunos2(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars1 = StarCollections.basicStars(workPath)
    val starsToShapable = shapabelsStarsToPoints(Color.yellow)(_)
    val min = 7.0
    val max = 9.0
    val starProb = 1.0
    oneShell(stars1, bc, min, max, starProb, starsToShapable)
  }

  def sunms1(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars1 = StarCollections.basicStars(workPath)
    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("05", 5.0, 5.2, 200 / 1000.0, shapabelsStarsToSpheres(0.04, Color.darkRed)(_)),
      ShellDef("08", 8.0, 8.2, 800 / 1000.0, shapabelsStarsToSpheres(0.06, Color.orange)(_)),
      ShellDef("11", 11.0, 11.2, 1000 / 1000.0, shapabelsStarsToSpheres(0.08, Color.yellow)(_)),
    )

    multiShells(stars1, bc, shells)
  }

  def sunms2(workPath: Path, bc: Color): Seq[Shapable] = {

    def equalDist(dist: Double)(star: Star): Star = star.copy(parallax = 1.0 / dist)

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("a", 5.0, 6.0, 400 / 1000.0, shapabelsStarsToPoints(Color.yellow)(_), equalDist(5.5) _),
      ShellDef("b", 8.0, 9.0, 700 / 1000.0, shapabelsStarsToPoints(Color.orange)(_), equalDist(8) _),
      ShellDef("c", 11.0, 12.0, 1000 / 1000.0, shapabelsStarsToPoints(Color.darkRed)(_), equalDist(10.5) _),
    )
    val stars = StarCollections.basicStars(workPath)
    multiShells(stars, bc, shells)
  }

  // TODO rename
  def sphere2(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val startProb = 0.0001
    val endProb = 0.1
    val shellCnt = 10
    val shellThikness = 0.2
    val startColor = Color.red
    val endColor = Color.yellow
    sphere(stars, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  // TODO rename
  def sphere5(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val shellCnt = 10
    val shellThikness = 0.5
    val startProb = 0.0001
    val endProb = 0.2
    val startColor = Color.blue
    val endColor = Color.green
    sphere(stars, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  // TODO rename
  def sphere8(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val shellCnt = 20
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.15
    val startColor = Color.darkRed
    val endColor = Color.orange
    sphere(stars, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  // TODO rename
  def sphere16(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars1 = StarCollections.basicStars(workPath)

    val shellCnt = 40
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.6
    val startColor = Color.orange
    val endColor = Color.green
    sphere(stars1, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

}

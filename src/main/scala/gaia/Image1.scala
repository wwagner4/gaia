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

  def dir01(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val ranges = Seq((7.9, 8.1))
    val colors = Palette.p2c10.colors
    val baseDirectionVec = Vec(1, 0, 1)
    val starShapes =
      stars.map(toStarPosDir)
        .filter(filterShells(ranges)(_))
        .map { s =>
          val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(ci)
          X3d.Shapable.Line(start = s.pos, end = s.pos.add(s.dir.mul(0.005)), startColor = bc, endColor = c)
        }
    println(s"filtered ${stars.size} stars")
    starShapes.toSeq
    ++ shapablesCoordinatesGray(5, bc)
    ++ shapablesCoordinatesGray(10, bc, offset = ImageUtil.galacicCenter)
  }

  def dir02(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
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
        X3d.Shapable.Line(start = s.pos, end = s.pos.add(s.dir.mul(0.003)), startColor = bc, endColor = c)
      }
    println(s"filtered ${starShapables.size} stars")
    starShapables.toSeq
    ++ shapablesCoordinatesGray(5, bc)
    ++ shapablesCoordinatesGray(10, bc, offset = ImageUtil.galacicCenter)
  }

  def dirs(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
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

    val ranges = (8 to 23)
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
    val starss = stars1.map(toStarPosDirGalactic)
      .filter(filterShells(ranges)(_))
      .map(adjust)
      .map { s =>
        val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
        val c: Color = colors(ci)
        X3d.Shapable.Sphere(position = s.pos, color = c, radius = 0.05)
      }
    println(s"filtered ${starss.size} stars")
    starss.toSeq
    ++ shapablesCoordinatesGray(5, bc)
    ++ shapablesCoordinatesGray(10, bc, offset = ImageUtil.galacicCenter)
  }

  def gc(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val rotation = Vec(degToRad(-4), degToRad(96), degToRad(0))
    Seq(Shapable.Circle(Vec(0, 0, 0), rotation = rotation, radius = 8))
    ++ shapablesCoordinatesColored(5, bc)
    ++ shapablesCoordinatesColored(10, bc, offset = ImageUtil.galacicCenter)
  }

  def nearSunVeloInner(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val minDist = 0.0
    val maxDist = 0.04
    val colors = Palette.p1c10.colors
    val lengthFactor = 0.8
    nearSunVelo(stars, bc, minDist, maxDist, colors, lengthFactor)
  }

  def nearSunDirtest(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
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

    val stars = stars1.filter { s =>
      val dist = 1 / s.parallax
      dist < maxDist && dist > minDist
    }
    println(s"There are ${stars.size} stars near the sun")
    shapabels(stars = stars).toSeq
    ++ shapablesCoordinatesColored(maxDist * 1.2, bc)
  }

  def nearSunVeloOuter(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val minDist = 0.04
    val maxDist = 0.05
    val colors = Palette.p2c10.colors
    val lengthFactor = 2.0
    nearSunVelo(stars, bc, minDist, maxDist, colors, lengthFactor)
  }

  def nearSunDirections27pc(stars: Iterable[Star], bc: Color): Seq[Shapable] = {

    val radius = 0.00005
    val maxDist = 0.027

    def shapabels(radius: Double)(stars: Iterable[Star]): Iterable[Shapable] = {
      stars
        .map(toStarPosDir)
        .flatMap { s =>
          val br = 1 - (s.pos.length / (maxDist * 1.2))
          val c = Color.orange.mul(br)
          Seq(Shapable.Cylinder(position = s.pos, rotation = s.dir, radius = radius, height = radius * 100, color = c))
        }
    }

    val sl = stars.filter(s => 1 / s.parallax < maxDist)
    println(s"There are ${sl.size} stars near the sun")
    shapabels(radius = radius)(stars = sl).toSeq
    ++ shapablesCoordinatesGray(maxDist * 1.2, bc)
  }


  def oneShellSpheres(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    val starsToShapable = shapabelsStarsToSpheres(0.02, Color.gray(1.0))(_)
    val min = 7.0
    val max = 9.0
    val starProb = 0.1
    oneShell(stars1, bc, min, max, starProb, starsToShapable)
  }

  def oneShellPoints(stars1: Iterable[Star], bc: Color): Seq[Shapable]  = {
    val starsToShapable = shapabelsStarsToPoints(Color.yellow)(_)
    val min = 7.0
    val max = 9.0
    val starProb = 1.0
    oneShell(stars1, bc, min, max, starProb, starsToShapable)
  }

  def shellsSphere(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("05", 5.0, 5.2, 200 / 1000.0, shapabelsStarsToSpheres(0.04, Color.darkRed)(_)),
      ShellDef("08", 8.0, 8.2, 800 / 1000.0, shapabelsStarsToSpheres(0.06, Color.orange)(_)),
      ShellDef("11", 11.0, 11.2, 1000 / 1000.0, shapabelsStarsToSpheres(0.08, Color.yellow)(_)),
    )

    multiShells(stars1, bc, shells)
  }

  def shellsPoints(stars: Iterable[Star], bc: Color): Seq[Shapable]  = {

    def equalDist(dist: Double)(star: Star): Star = star.copy(parallax = 1.0 / dist)

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("a", 5.0, 6.0, 400 / 1000.0, shapabelsStarsToPoints(Color.yellow)(_), equalDist(5.5) _),
      ShellDef("b", 8.0, 9.0, 700 / 1000.0, shapabelsStarsToPoints(Color.orange)(_), equalDist(8) _),
      ShellDef("c", 11.0, 12.0, 1000 / 1000.0, shapabelsStarsToPoints(Color.darkRed)(_), equalDist(10.5) _),
    )

    multiShells(stars, bc, shells)
  }

  def sphere2(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    val startProb = 0.0001
    val endProb = 0.1
    val shellCnt = 10
    val shellThikness = 0.2
    val startColor = Color.red
    val endColor = Color.yellow
    sphere(stars1, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sphere5(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    val shellCnt = 10
    val shellThikness = 0.5
    val startProb = 0.0001
    val endProb = 0.2
    val startColor = Color.blue
    val endColor = Color.green
    sphere(stars1, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sphere8(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    val shellCnt = 20
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.15
    val startColor = Color.darkRed
    val endColor = Color.orange
    sphere(stars1, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sphere16(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    val shellCnt = 40
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.6
    val startColor = Color.orange
    val endColor = Color.green
    sphere(stars1, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

}

package gaia

import com.sun.imageio.plugins.common.BogusColorSpace
import gaia.Data.Star
import gaia.X3d.Shapable
import gaia.X3d.Shapable.Line

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Base64
import scala.util.Random


object Image1 {
  
  import X3d._
  import ImageUtil._

  def dir01(id: String, workPath: Path): Unit = {
    val bgColor = gaiaImage(id).backColor
    val ranges = Seq((7.9, 8.1))

    def allShapables(bc: Color): Seq[Shapable] = {
      println(s"running $id")

      val colors = Palette.p2c10.colors
      val baseDirectionVec = Vec(1, 0, 1)
      val stars = basicStars(workPath)
        .map(StarPosDir.fromStar)
        .filter(filterShells(ranges)(_))
        .map { s =>
          val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(ci)
          X3d.Shapable.Line(start = s.pos, end = s.pos.add(s.dir.mul(0.005)), startColor = bc, endColor = c)
        }
      println(s"filtered ${stars.size} stars")
      stars
      ++ shapablesCoordinatesGray(5, bgColor)
      ++ shapablesCoordinatesGray(10, bgColor, offset = Util.galacicCenter)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }

  def dir02(id: String, workPath: Path): Unit = {
    val bgColor = gaiaImage(id).backColor
    val ranges = Seq(
      (9.6, 10.0),
      (7.8, 8.0),
      (5.95, 6.0),
    )

    def allShapables(bc: Color): Seq[Shapable] = {
      println(s"running $id")

      def adjust(star: StarPosDir): StarPosDir = {
        val dist = star.pos.length
        val dist1 = math.ceil(dist)
        val pos1 = star.pos.norm.mul(dist1)
        val dir1 = star.dir.norm.mul(-1.0)
        star.copy(pos = pos1, dir = dir1)
      }

      val colors = Palette.p5c8.colors
      val baseDirectionVec = Vec(1, -1, 1)
      val stars = basicStars(workPath)
        .map(StarPosDir.fromStar)
        .filter(filterShells(ranges)(_))
        .map { s =>
          val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c = colors(ci)
          X3d.Shapable.Line(start = s.pos, end = s.pos.add(s.dir.mul(0.003)), startColor = bc, endColor = c)
        }
      println(s"filtered ${stars.size} stars")
      stars
      ++ shapablesCoordinatesGray(5, bgColor)
      ++ shapablesCoordinatesGray(10, bgColor, offset = Util.galacicCenter)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }

  def dirs(id: String, workPath: Path): Unit = {
    val epsBase = 0.4
    val bgColor = gaiaImage(id).backColor
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

    def analyse(stars: Seq[X3d.Shapable.Sphere]): Unit = {
      val grouped = stars
        .groupBy(s => math.round(s.position.length).toInt)
        .map { case (v, l) => (v, l.size) }
        .toSeq
        .sortBy(t => t._1)
      println("-- grouped size: " + grouped.size)
      grouped.foreach(g => printf("%5d %5d%n", g._1, g._2))
    }


    def allShapables(bc: Color): Seq[Shapable] = {
      println(s"running $id")

      def adjust(star: StarPosDir): StarPosDir = {
        val dist = star.pos.length
        val dist1 = math.ceil(dist)
        val pos1 = star.pos.norm.mul(dist1)
        star.copy(pos = pos1)
      }

      val colors = Palette.p5c8.colors
      val baseDirectionVec = Vec(1, -1, 1)
      val stars = basicStars(workPath)
        .map(StarPosDir.fromStar)
        .filter(filterShells(ranges)(_))
        .map(adjust)
        .map { s =>
          val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
          val c: Color = colors(ci)
          X3d.Shapable.Sphere(position = s.pos, color = c, radius = 0.05)
        }
      println(s"filtered ${stars.size} stars")
      analyse(stars)
      stars
      ++ shapablesCoordinatesGray(5, bgColor)
      ++ shapablesCoordinatesGray(10, bgColor, offset = Util.galacicCenter)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }

  def gc(id: String, workPath: Path): Unit = {
    val bgColor = gaiaImage(id).backColor
    val rotation = Vec(X3d.degToRad(-4), X3d.degToRad(96), X3d.degToRad(0))

    def allShapables(bc: Color): Seq[Shapable] = {
      Seq(Shapable.Circle(Vec(0, 0, 0), rotation = rotation, radius = 8))
      ++ shapablesCoordinatesColored(5, bgColor)
      ++ shapablesCoordinatesColored(10, bgColor, offset = Util.galacicCenter)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }

  def nearSunVeloInner(id: String, workPath: Path): Unit = {
    val minDist = 0.0
    val maxDist = 0.04
    val colors = Palette.p1c10.colors
    val lengthFactor = 0.8
    nearSunVelo(id, workPath, minDist, maxDist, colors, lengthFactor)
  }

  def nearSunVeloOuter(id: String, workPath: Path): Unit = {
    val minDist = 0.04
    val maxDist = 0.05
    val colors = Palette.p2c10.colors
    val lengthFactor = 1.0
    nearSunVelo(id, workPath, minDist, maxDist, colors, lengthFactor)
  }

  def nearSunDirections27pc(id: String, workPath: Path): Unit = {

    val radius = 0.00005
    val maxDist = 0.027
    val bgColor = gaiaImage(id).backColor

    def shapabels(radius: Double)(stars: Iterable[Star]): Iterable[Shapable] = {
      stars
        .map(StarPosDir.fromStar)
        .flatMap { s =>
          val br = 1 - (s.pos.length / (maxDist * 1.2))
          // println(s)
          val c = Color.orange.mul(br)
          Seq(Shapable.Cylinder(position = s.pos, rotation = s.dir, radius = radius, height = radius * 100, color = c))
        }
    }

    def allShapables(bgColor: Color): Seq[Shapable] = {
      val stars = nearSunStars(workPath).filter(s => 1 / s.parallax < maxDist)
      println(s"There are ${stars.size} stars near the sun")
      shapabels(radius = radius)(stars = stars).toSeq
      ++ shapablesCoordinatesGray(maxDist * 1.2, bgColor)
    }

    createX3dFile(id, workPath, bgColor, allShapables)

  }


  def oneShellSpheres(id: String, workPath: Path): Unit = {
    val starsToShapable = shapabelsStarsToSpheres(0.02, Color.gray(1.0))(_)
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
      ShellDef("05", 5.0, 5.2, 200 / 1000.0, shapabelsStarsToSpheres(0.04, Color.darkRed)(_)),
      ShellDef("08", 8.0, 8.2, 800 / 1000.0, shapabelsStarsToSpheres(0.06, Color.orange)(_)),
      ShellDef("11", 11.0, 11.2, 1000 / 1000.0, shapabelsStarsToSpheres(0.08, Color.yellow)(_)),
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

}

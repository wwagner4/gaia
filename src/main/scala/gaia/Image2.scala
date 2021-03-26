package gaia

import java.io.File
import java.nio.file.{Files, Path}
import java.util.Locale
import scala.util.Random

object Image2 {

  import Data.Star
  import ImageUtil._
  import X3d._
  import Vector._

  def gc1(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    println("running around the galactic center")

    val maxDist = 1.7
    val cols: Seq[Color] = X3d.Palette.p9c6.colors
    val cnt = cols.size
    val dens = 1

    def colorForDistance(s: StarPosDir): Color = {
      val i = math.floor(cnt * s.pos.length / maxDist).toInt
      cols(i)
    }

    val spd = stars
      .map(toStarPosDirGalactic)
      .filter(s => Random.nextDouble() <= dens && s.pos.length < maxDist)
    println(s"filtered ${spd.size} stars")

    val starShapes = spd.map(s => Shapable.Sphere(s.pos, radius = 0.01, color = colorForDistance(s)))
    val sun = toGalacticCoords(ImageUtil.galacicCenter.mul(-1))

    starShapes.toList 
    ++ (2 to(25, 2)).map { r =>
      Shapable.Circle(translation = Vec.zero,
        rotation = Vec(0, degToRad(90), 0),
        color = Color.gray(0.1), radius = r * 0.1)
    }
    ++ shapablesCoordinatesOneColor(2, Color.gray(0.5), bc)
  }

  def gcd1(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    println("running around the galactic center")

    val maxDist = 3
    val cols: Seq[Color] = X3d.Palette.p9c6.colors
    val cnt = cols.size
    val dens = 0.1

    def colorForDistance(s: StarPosDir): Color = {
      val i = math.floor(cnt * s.pos.length / maxDist).toInt
      cols(i)
    }

    val stars = stars1.map(toStarPosDirGalactic)
      .filter(s => Random.nextDouble() <= dens && s.pos.length < maxDist)
    println(s"filtered ${stars.size} stars")

    val baseDirectionVec = Vec(1, -1, 1)
    val colors = Palette.p5c8.colors
    val starShapes = stars.toList.flatMap { s =>
      val d = s.dir.mul(0.0002)
      val e = s.pos.add(d)
      val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
      val c = colors(ci)
      Seq(
        Shapable.Sphere(position = s.pos, color = c, radius = 0.015),
        Shapable.Line(start = s.pos, end = e, startColor = c, endColor = bc))
    }

    val sphereShapes = (1 to(30, 5)).map { r =>
      Shapable.Circle(translation = Vec.zero,
        rotation = Vec(0, degToRad(90), 0),
        color = Color.gray(0.1), radius = r * 0.1)
    }
    val coordshapes = shapablesCoordinatesOneColor(4, Color.gray(0.2), bc)

    starShapes ++ sphereShapes ++ coordshapes
  }

  def gcd2(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val maxDist = 2.0
    val colors = Palette.p5c8.colors
    val baseDirectionVec = Vec(1, 1, 1)
    val shapes = stars
      .map(toStarPosDirGalactic)
      .filter(_ => Random.nextDouble() < 0.3)
      .filter(s => s.pos.length < maxDist)
      .toSeq
      .map{s =>
        val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
        ImageUtil.shapeCone(color = colors(ci), lengthFactor = 0.001, geo= Geo.Absolute(0.01))(s)}
    println(s"created ${shapes.size} shapes")
    shapes
    ++ (10 to(40, 10)).map { r =>
      Shapable.Circle(translation = Vec.zero,
        rotation = Vec(0, degToRad(90), 0),
        color = Color.gray(0.1), radius = r * 0.1)
    }
    ++ shapablesCoordinatesOneColor(3, Color.gray(0.4), bc)

  }

  def dens(stars1: Iterable[Star], bc: Color): Seq[Shapable] = {
    println("running density")

    val cubeSize = 16
    val cubeCount = 16

    val stars = stars1.map(toStarPosDirGalactic)
      .filter(s => Random.nextDouble() <= 0.01 && s.pos.length < cubeSize)

    val ic = inCube(cubeSize, cubeCount) _
    val counts = for (i <- -cubeCount until cubeCount;
                      j <- -cubeCount until cubeCount;
                      k <- -cubeCount until cubeCount) yield {
      val sc = stars.map { s => if (ic(s.pos, i, j, k)) 1 else 0 }
      ((i, j, k), sc.sum)
    }
    val maxCount = counts.map { case (_, v) => v }.max
    println(s"size max: ${counts.size} $maxCount")

    val densities = counts.map { case (k, v) => (k, math.sqrt(v.toDouble / maxCount)) }

    val shapes = densities
      .flatMap { case (k, v) =>
        if (v <= 0.01) {
          None
        }
        else {
          val pos = Vec(k._1 + 0.5, k._2 + 0.5, k._3 + 0.5)
          Some(Shapable.Sphere(position = pos, color = Color.green, radius = v * 0.6))
        }
      }
    shapes
  }

  def shapesLines(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    stars.toSeq
      .map(toStarPosDir)
      .map(shapeLine(bc, Color.green))
  }

  def shapesCones(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val shapes = stars.toSeq
      .map(toStarPosDir)
      .map(shapeCone(Color.white))
    println(s"created ${shapes.size} cones")
    shapes
  }

  def shapesCyl(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val shapes = stars.toSeq
      .map(toStarPosDir)
      .map(shapeCylinder(Color.white))
    println(s"created ${shapes.size} cones")
    shapes
  }

  def ptoc(stars: Iterable[Star], bc: Color): Seq[Shapable] = {
    val as = stars.toSeq
      .map { star =>
        val a = Vec.zero
        val p = PolarVec(1 / star.parallax, degToRad(star.ra), degToRad(star.dec))
        val b = p.toVec
        println(s"star.ra: ${star.ra} -- p:${p} -- b:${b}")
        Shapable.Line(start = a, end = b, startColor = bc, endColor = Color.green)
      }
    as ++ shapablesCoordinatesColored(len = 50, bgColor = bc)
  }

}
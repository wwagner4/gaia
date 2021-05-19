package gaia

import com.sun.imageio.plugins.common.BogusColorSpace

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Base64
import scala.util.Random

/**
 * Creates X3d images as a sequence of Shapable
 */
object ImageFactory {

  import X3d._
  import ImageUtil._
  import Data._
  import Vector._
  import Util._

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
          Shapable.Cone(color = c, position = s.pos, radius = 0.01, direction = s.dir.mul(0.005))
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
        Shapable.Cone(color = c, position = s.pos, radius = 0.03, direction = s.dir.mul(0.003))
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

    val ranges = (5 to(20, 5))
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
  }

  def sund1(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.nearSunStars(workPath)
    val minDist = 0.0
    val maxDist = 0.03
    val colors = Palette.p1c10.colors
    val lengthFactor = 0.00003
    val radius = 0.00007
    nearSunVelo(stars, bc, minDist, maxDist, colors, lengthFactor, radius)
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
  }

  def sund3(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.nearSunStars(workPath)

    val minDist = 0.04
    val maxDist = 0.05
    val colors = Palette.p2c10.colors
    val lengthFactor = 0.00008
    val radius = 0.00006
    nearSunVelo(stars, bc, minDist, maxDist, colors, lengthFactor, radius)
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
          val c = Color.orange.brightness(br)
          Shapable.Cylinder(color = c, position = s.pos, radius = 1.0, direction = s.dir.mul(0.00005))
        }
    }

    val sl = stars.filter(s => 1 / s.parallax < maxDist)
    println(s"There are ${sl.size} stars near the sun")
    shapabels(radius = radius)(stars = sl).toSeq
      ++ shapablesCoordinatesGray(maxDist * 1.2, bc)
  }


  def sunos1(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars1 = StarCollections.basicStars(workPath)
    val starsToShapable = shapabelsStarsToSpheres(0.01, Color.gray(1.0))(_)
    val min = 7.0
    val max = 9.0
    val starProb = 0.5
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

  def sunnear1(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val startProb = 0.0001
    val endProb = 0.1
    val shellCnt = 10
    val shellThikness = 0.2
    val startColor = Color.red
    val endColor = Color.yellow
    sphere(stars, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sunnear2(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val shellCnt = 10
    val shellThikness = 0.5
    val startProb = 0.0001
    val endProb = 0.2
    val startColor = Color.blue
    val endColor = Color.green
    sphere(stars, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sunnear3(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)

    val shellCnt = 20
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.15
    val startColor = Color.darkRed
    val endColor = Color.orange
    sphere(stars, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def sun16(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars1 = StarCollections.basicStars(workPath)

    val shellCnt = 40
    val shellThikness = 0.4
    val startProb = 0.0001
    val endProb = 0.6
    val startColor = Color.orange
    val endColor = Color.green
    sphere(stars1, bc, shellCnt, shellThikness, startProb, endProb, startColor, endColor)
  }

  def gc1(workPath: Path, bc: Color): Seq[Shapable] = {
    println("running around the galactic center")
    val stars = StarCollections.basicStars(workPath)

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

    val circleShapes = {
      (2 to(25, 2)).map { r =>
        Shapable.Circle(translation = Vec.zero,
          rotation = Vec(0, 0, 0),
          color = Color.gray(0.1), radius = r * 0.1)
      }
    }
    val coordShapes = shapablesCoordinatesOneColor(2, Color.gray(0.5), bc)

    starShapes.toList ++ circleShapes ++ coordShapes
  }

  def gcd1(workPath: Path, bc: Color): Seq[Shapable] = {
    println("running around the galactic center")
    val stars = StarCollections.basicStars(workPath)

    val maxDist = 3
    val cols: Seq[Color] = X3d.Palette.p9c6.colors
    val cnt = cols.size
    val dens = 0.2

    def colorForDistance(s: StarPosDir): Color = {
      val i = math.floor(cnt * s.pos.length / maxDist).toInt
      cols(i)
    }

    val starsFiltered = stars.map(toStarPosDirGalactic)
      .filter(s => Random.nextDouble() <= dens && s.pos.length < maxDist)
    println(s"filtered ${starsFiltered.size} stars")

    val baseDirectionVec = Vec(1, -1, 1)
    val colors = Palette.p5c8.colors
    val starShapes = starsFiltered.toList.flatMap { s =>
      val d = s.dir.mul(0.0002)
      val e = s.pos.add(d)
      val ci = math.floor(s.dir.angle(baseDirectionVec) * colors.size / 180).toInt
      val c = colors(ci)
      Seq(
        Shapable.Sphere(position = s.pos, color = c, radius = 0.015),
        Shapable.Line(start = s.pos, end = e, startColor = c, endColor = bc))
    }

    val coordshapes = shapablesCoordinatesOneColor(4, Color.gray(0.4), bc)

    starShapes ++ ImageUtil.circleShapes(40, 8) ++ coordshapes
  }

  def gcd2(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)
    val maxDist = 2.0
    val shapes = stars
      .map(toStarPosDirGalactic)
      .filter(_ => Random.nextDouble() < 0.7)
      .filter(s => s.pos.length < maxDist)
      .toSeq
      .map { s =>
        val col = ImageUtil.colorFromDirection(s.dir.x, s.dir.y)
        Shapable.Cone(color = col, position = s.pos, radius = 0.006, direction = s.dir.mul(0.001))
      }
    println(s"created ${shapes.size} shapes")

    val coordShapes = shapablesCoordinatesOneColor(3, Color.gray(0.6), bc)

    shapes ++ circleShapes(28, 14) ++ coordShapes

  }

  def gcd4(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)
    val maxDist = 4.0

    def horzontalSlice(z1: Double, z2: Double)(star: StarPosDir): Boolean = {
      def between(x: Double, a: Double, b: Double): Boolean = {
        if a > b then x <= a && x >= b
        else x <= b && x >= a
      }

      between(star.pos.z, z1, z2)
    }

    def sliceToMean(slices: Seq[(Double, Double)])(star: StarPosDir): Option[Double] = {
      if slices.isEmpty then None
      else {
        val a = slices.head._1
        val b = slices.head._2
        if horzontalSlice(a, b)(star) then Some((a + b) / 2.0)
        else sliceToMean(slices.tail)(star)
      }
    }

    def slizeToShapes(mean: Double, stars: Seq[StarPosDir]): Seq[Shapable] = {
      stars
        .map { s =>
          val color = ImageUtil.colorFromDirection(s.dir.x, s.dir.y)
          Shapable.Cone(color = color, position = s.pos.copy(z = mean),
            radius = 0.006, direction = s.dir.mul(0.001))
        }
    }

    val slices = Seq(
      (4.0, 4.5),
      (3.0, 3.5),
      (2.0, 2.5),
      (1.0, 1.5),
      (-1.0, -1.5),
      (-2.0, -2.5),
      (-3.0, -3.5),
      (-4.0, -4.5),
    )

    val starSlices: Seq[(Double, Seq[StarPosDir])] = stars
      .map(toStarPosDirGalactic)
      .filter(s => s.pos.length < maxDist)
      .groupBy(sliceToMean(slices))
      .flatMap((a, b) => a.map(m => (m, b)))
      .toSeq

    val shapes = starSlices
      .flatMap((mean, stars) => slizeToShapes(mean, stars))

    val nst = shapes.size
    val nsl = slices.size
    println(s"found $nst stars in $nsl slices")

    shapes ++ circleShapes(maxDist * 1.5, 12, color = Color.gray(0.4))

  }

  def gcd5(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)
    val maxDist = 2.0

    def horzontalSlice(z1: Double, z2: Double)(star: StarPosDir): Boolean = {
      def between(x: Double, a: Double, b: Double): Boolean = {
        if a > b then x <= a && x >= b
        else x <= b && x >= a
      }

      between(star.pos.z, z1, z2)
    }

    def sliceToMean(slices: Seq[(Double, Double)])(star: StarPosDir): Option[Double] = {
      if slices.isEmpty then None
      else {
        val a = slices.head._1
        val b = slices.head._2
        if horzontalSlice(a, b)(star) then Some((a + b) / 2.0)
        else sliceToMean(slices.tail)(star)
      }
    }

    def slizeToShapes(mean: Double, stars: Seq[StarPosDir]): Seq[Shapable] = {
      stars
        .map { s =>
          val color = ImageUtil.colorFromDirection(s.dir.x, s.dir.y, palette = Palette.p1c10)
          Shapable.Cone(color = color, position = s.pos.copy(z = mean),
            radius = 0.006, direction = s.dir.mul(0.001))
        }
    }

    val slices = Seq(
      (-1.0, 1.0),
    )

    val starSlices: Seq[(Double, Seq[StarPosDir])] = stars
      .map(toStarPosDirGalactic)
      .filter(s => s.pos.length < maxDist)
      .groupBy(sliceToMean(slices))
      .flatMap((a, b) => a.map(m => (m, b)))
      .toSeq

    val shapes = starSlices
      .flatMap((mean, stars) => slizeToShapes(mean, stars))

    val nst = shapes.size
    val nsl = slices.size
    println(s"found $nst stars in $nsl slices")

    shapes ++ circleShapes(maxDist * 1.3, 20, color = Color.gray(0.2))

  }

  def gc3(workPath: Path, bc: Color): Seq[Shapable] = {
    val stars = StarCollections.basicStars(workPath)
    val maxDist = 1.6
    val colors = Palette.p5c8.colors
    val gcstars = stars
      .map(toStarPosDirGalactic)
      .filter(s => s.pos.length < maxDist)
      .toSeq

    def stat(): Unit = {
      val (min, mean, max) = {
        val value = gcstars.map(_.dir.length)
        val sum = value.sum
        (value.min, sum / value.size, value.max)
      }

      println(min)
      println(mean)
      println(max)
    }

    val shapes = gcstars
      .map { s =>
        val ci = Math.floor(s.pos.length / maxDist * colors.size).toInt
        val radius = s.dir.length / 727 * 0.04
        Shapable.Sphere(color = colors(ci), position = s.pos, radius = radius)
      }
    println(s"created ${shapes.size} shapes")

    shapes ++ circleShapes(28, 7)
  }

  def gcs1(workPath: Path, bc: Color): Seq[Shapable] = {
    def maxRadius = 5.0

    def testStars: Seq[StarPosDir] = {

      def inCylinder(radius: Double, thikness: Double)(star: StarPosDir): Boolean = {
        val t2 = thikness / 2.0
        val dxy = math.sqrt(star.pos.x * star.pos.x + star.pos.y * star.pos.y)
        dxy <= radius && star.pos.z <= t2 && star.pos.z >= -t2
      }

      val stars = StarCollections.basicStars(workPath).filter(_ => Random.nextDouble() < 0.7)
      stars.map(toStarPosDirGalactic).filter(inCylinder(maxRadius, maxRadius * 0.8))
    }

    def starToSector(sectors: Seq[Sector])(star: StarPosDir): Sector = {
      def inSector(star: StarPosDir, sector: Sector): Boolean = {
        val a = Util.angle2DDeg(star.pos.x, star.pos.y)
        a >= sector.startDeg && a <= sector.endDeg
      }

      if sectors.isEmpty then {
        throw IllegalArgumentException("A star must always be in a sector. Check if your sectors are complete")
      }
      else {
        if inSector(star, sectors.head) then sectors.head
        else starToSector(sectors.tail)(star)
      }
    }

    val sectors = Util.sectors(10)
    val starsGrouped = testStars
      .groupBy(starToSector(sectors))
      .toSeq

    val minGroupSize = starsGrouped.map((_, sl) => sl.size).min

    starsGrouped
      .sortBy(x => x._1.startDeg)
      .foreach((s, stars) => println(f"${s}%20s - ${stars.size}%4d"))

    println(s"min group size $minGroupSize")

    val starsEqual: Seq[(Sector, Seq[StarPosDir])] = starsGrouped.flatMap { (s, ls) =>
      val p = minGroupSize.toDouble / ls.size
      if p < 0.01 then None
      else {
        val fs = ls.filter(_ => Random.nextDouble() < p)
        Some((s, fs))
      }
    }.toSeq.sortBy(x => x._1.startDeg)

    starsEqual
      .foreach((s, stars) => println(f"${s}%20s - ${stars.size}%4d"))

    val colors = Palette.p1c10.lazyColors
    val bc = Color.veryDarkBlue
    val shapablesStars = starsEqual
      .zip(colors)
      .flatMap(t => (t._1._2.zip(LazyList.continually(t._2))))
      .map(t => Shapable.Sphere(position = t._1.pos, color = t._2, radius = 0.02))
    println(s"selected ${shapablesStars.size} stars")

    val cn = Color.orange.brightness(0.4)
    shapablesStars
      ++ ImageUtil.circleShapes(maxRadius * 1.3, 5, color = cn)
      ++ Seq(Shapable.Line(start = Vec(0, 0, maxRadius), end = Vec(0, 0, -maxRadius), startColor = cn, endColor = cn))
    //++ shapablesCoordinatesColored(len = 10, bgColor = bc)
  }

  def gcs2(workPath: Path, bc: Color): Seq[Shapable] = {
    def maxRadius = 8.0

    def testStars: Seq[StarPosDir] = {

      def inCylinder(radius: Double, zStart: Double, zStop: Double)(star: StarPosDir): Boolean = {
        val (t1, t2) = if zStart < zStop then (zStart, zStop) else (zStop, zStart)
        val dxy = math.sqrt(star.pos.x * star.pos.x + star.pos.y * star.pos.y)
        dxy <= radius && star.pos.z <= t2 && star.pos.z >= t1
      }

      val stars = StarCollections.basicStars(workPath).filter(_ => Random.nextDouble() < 1.0)
      stars.map(toStarPosDirGalactic).filter(inCylinder(maxRadius, 2.0, 3.0))
    }

    def starToSector(sectors: Seq[Sector])(star: StarPosDir): Sector = {
      def inSector(star: StarPosDir, sector: Sector): Boolean = {
        val a = Util.angle2DDeg(star.pos.x, star.pos.y)
        a >= sector.startDeg && a <= sector.endDeg
      }

      if sectors.isEmpty then {
        throw IllegalArgumentException("A star must always be in a sector. Check if your sectors are complete")
      }
      else {
        if inSector(star, sectors.head) then sectors.head
        else starToSector(sectors.tail)(star)
      }
    }

    val sectors = Util.sectors(10)
    val starsGrouped = testStars
      .groupBy(starToSector(sectors))
      .toSeq

    val minGroupSize = starsGrouped.map((_, sl) => sl.size).min

    starsGrouped
      .sortBy(x => x._1.startDeg)
      .foreach((s, stars) => println(f"${s}%20s - ${stars.size}%4d"))

    println(s"min group size $minGroupSize")

    val starsEqual: Seq[(Sector, Seq[StarPosDir])] = starsGrouped.flatMap { (s, ls) =>
      val p = minGroupSize.toDouble / ls.size
      if p < 0.01 then None
      else {
        val fs = ls.filter(_ => Random.nextDouble() < p)
        Some((s, fs))
      }
    }.toSeq.sortBy(x => x._1.startDeg)

    starsEqual
      .foreach((s, stars) => println(f"${s}%20s - ${stars.size}%4d"))

    val colors = Palette.p1c10.lazyColors
    val bc = Color.veryDarkBlue
    val shapablesStars = starsEqual
      .zip(colors)
      .flatMap(t => (t._1._2.zip(LazyList.continually(t._2))))
      .map(t => Shapable.Sphere(position = t._1.pos, color = t._2, radius = 0.02))
    println(s"selected ${shapablesStars.size} stars")

    val cn = Color.orange.brightness(0.2)
    shapablesStars
      ++ ImageUtil.circleShapes(maxRadius * 1.3, 5, color = cn)
      ++ Seq(Shapable.Line(start = Vec(0, 0, maxRadius), end = Vec(0, 0, -maxRadius), startColor = cn, endColor = cn))
    //++ shapablesCoordinatesColored(len = 10, bgColor = bc)
  }

  def dens(workPath: Path, bc: Color): Seq[Shapable] = {
    println("running density")
    val stars = StarCollections.basicStars(workPath)

    val cubeSize = 16
    val cubeCount = 16

    val starsFiltered = stars.map(toStarPosDirGalactic)
      .filter(s => Random.nextDouble() <= 0.01 && s.pos.length < cubeSize)

    val ic = inCube(cubeSize, cubeCount) _
    val counts = for (i <- -cubeCount until cubeCount;
                      j <- -cubeCount until cubeCount;
                      k <- -cubeCount until cubeCount) yield {
      val sc = starsFiltered.map { s => if (ic(s.pos, i, j, k)) 1 else 0 }
      ((i, j, k), sc.sum)
    }
    val maxCount = counts.map { case (_, v) => v }.max
    println(s"size max: ${counts.size} $maxCount")

    val densities = counts.map { case (k, v) => (k, math.sqrt(v.toDouble / maxCount)) }

    val shapes = densities
      .flatMap { (k, v) =>
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


}

package gaia

import gaia.Data.Star
import gaia.ImageUtil.{StarPosDir, basicStars, createX3dFile, gaiaImage, shapablesCoordinatesGray, shapablesCoordinatesColored, toStarPosDirGalactic}
import gaia.X3d.{Color, Shapable, Vec}

import java.io.File
import java.nio.file.{Files, Path}
import java.util.Locale
import scala.util.Random

object Image2 {

  def aroundGalacticCenter(id: String, workPath: Path): Unit = {
    println("running around the galactic center")

    val maxDist = 5
    val cols: Seq[Color] = X3d.Palette.p9c6.colors
    val cnt = cols.size
    val dens = 0.05

    def colorForDistance(s: StarPosDir): Color = {
      val i = math.floor(cnt * s.pos.length / maxDist).toInt
      cols(i)
    }

    def draw(bgColor: Color): Seq[Shapable] = {
      val stars = basicStars(workPath)
        .map(toStarPosDirGalactic)
        .filter(s => Random.nextDouble() <= dens && s.pos.length < maxDist)
      println(s"filtered ${stars.size} stars")
      val poss = stars.map(s => s.pos)
      val starShapes = Seq(Shapable.PointSet(positions = poss, color = Color.gray(0.5)))
      //val starShapes = stars.map(s => Shapable.Sphere(translation = s.pos, radius = 0.005, color = colorForDistance(s)))
      starShapes
      ++ Seq (Shapable.Circle(translation = Vec.zero, radius = 7.0))
      ++ shapablesCoordinatesColored(3, bgColor)
      ++ shapablesCoordinatesColored(3, bgColor, Util.galacicCenter.mul(-1))

    }

    createX3dFile(id, workPath, gaiaImage(id).backColor, draw)
  }

  def dens(id: String, workPath: Path): Unit = {
    println("running density")

    val cubeSize = 16
    val cubeCount = 16

    def writeDensities(densities: IndexedSeq[((Int, Int, Int), Double)]) = {
      def d2Str(d: ((Int, Int, Int), Double)): Option[String] = {
        val dv = d._2
        if (dv < 0.000001) None
        else Some("%d,%d,%d,%.6f".formatLocal(Locale.ENGLISH, d._1._1, d._1._2, d._1._3, d._2))
      }

      val h = Seq("x", "y", "z", "dens", "\n").mkString(",")
      val v = densities.flatMap(d2Str).mkString("\n")
      val all = h + v
      val fnam = "densities.csv"
      val outDir = workPath.resolve("data")
      if (Files.notExists(outDir))
        Files.createDirectories(outDir)
      val outFile = outDir.resolve(fnam)
      Util.writeString(outFile, all)
      println(s"Wrote densities to ${outFile.toAbsolutePath}")
    }

    val stars = basicStars(workPath)
      .map(toStarPosDirGalactic)
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
    writeDensities(densities)

    def draw(bgColor: Color): Seq[Shapable] = {
      val shapes = densities
        .flatMap { case (k, v) =>
          if (v <= 0.01) {
            val pos = Vec(k._1 + 0.5, k._2 + 0.5, k._3 + 0.5)
            Some(Shapable.Sphere(translation = pos, color = Color.gray(0.4), radius = 0.01 * 0.6))
          }
          else {
            val pos = Vec(k._1 + 0.5, k._2 + 0.5, k._3 + 0.5)
            Some(Shapable.Sphere(translation = pos, color = Color.green, radius = v * 0.6))
          }
        }
      shapes
      ++ shapablesCoordinatesGray(3, bgColor)
    }

    createX3dFile(id, workPath, gaiaImage(id).backColor, draw)
  }

  def inCube(cubeSize: Int, cubeCount: Int)(pos: Vec, i: Int, j: Int, k: Int): Boolean = {
    val ix = math.floor(pos.x * cubeCount / cubeSize).toInt
    val iy = math.floor(pos.y * cubeCount / cubeSize).toInt
    val iz = math.floor(pos.z * cubeCount / cubeSize).toInt
    ix == i && iy == j && iz == k
  }


}

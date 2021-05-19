package gaia


import java.nio.file.Path
import scala.util.Random

object DataUtil {

  import ImageUtil._
  import Util._

  def starsEqualPerSector(workPath: Path, sectorCnt: Int, maxRadius: Double, zStart: Double, zStop: Double,
                          density: Double): Seq[Seq[StarPosDir]] = {

    def testStars: Seq[StarPosDir] = {

      def inCylinder(radius: Double, zStart: Double, zStop: Double)(star: StarPosDir): Boolean = {
        val (t1, t2) = if zStart < zStop then (zStart, zStop) else (zStop, zStart)
        val dxy = math.sqrt(star.pos.x * star.pos.x + star.pos.y * star.pos.y)
        dxy <= radius && star.pos.z <= t2 && star.pos.z >= t1
      }

      val stars = StarCollections.basicStars(workPath).filter(_ => Random.nextDouble() < density)
      stars.map(toStarPosDirGalactic).filter(inCylinder(maxRadius, zStart, zStop))
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

    val sectors = Util.sectors(sectorCnt)
    val starsGrouped = testStars
      .groupBy(starToSector(sectors))
      .toSeq

    val minGroupSize = starsGrouped.map((_, sl) => sl.size).min

    starsGrouped.flatMap { (s, ls) =>
      val p = minGroupSize.toDouble / ls.size
      if p < 0.01 then None
      else {
        val fs = ls.filter(_ => Random.nextDouble() < p)
        Some((s, fs))
      }
    }.toSeq.sortBy(x => x._1.startDeg).map(t => t._2)

  }


}

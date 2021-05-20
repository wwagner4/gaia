package gaia


import java.nio.file.Path
import scala.util.Random

object DataUtil {

  import ImageUtil._
  import Util._

  sealed trait Region {
    def id: String
    def contains(star: StarPosDir): Boolean
  }

  case class Cylinder(id: String, radius: Double, z1: Double, z2: Double) extends Region {

    def contains(star: StarPosDir): Boolean = {
      val (t1, t2) = if z1 < z2 then (z1, z2) else (z2, z1)
      val dxy = math.sqrt(star.pos.x * star.pos.x + star.pos.y * star.pos.y)
      dxy <= radius && star.pos.z <= t2 && star.pos.z >= t1
    }

    override def toString: String = f"CYL($id $radius%.1f $z1%.1f $z2%.1f)"
  }

  case class Sector(startDeg: Int, endDeg: Int)

  def createSectors(cnt: Int): Seq[Sector] = {
    require(cnt > 0, "There must be at least one sector")
    require(cnt <= 360, "More than 360 sectors make no sense")
    val dist = 360.0 / cnt

    def borders(actBorder: Double, result: List[Int]): List[Int] = {
      if actBorder > 360.0000001 then result.reverse
      else borders(actBorder + dist, math.ceil(actBorder).toInt :: result)
    }

    val bs = borders(0.0, List())
    val bs1 = bs.tail
    bs.zip(bs1).map((from, to) => Sector(from, to - 1))
  }



  def starsAroundGalacticCenter(workPath: Path, density: Double): Seq[StarPosDir] = {

    StarCollections.basicStars(workPath)
      .filter(_ => Random.nextDouble() < density)
      .map(toStarPosDirGalactic)
  }

  def starsPerRegion(stars: Seq[StarPosDir], regions: Seq[Region]): Seq[(Region, Seq[StarPosDir])] = {

    def assign(regions: Seq[Region])(starPosDir: StarPosDir): Option[Region] = {
      if regions.isEmpty then None
      else if regions.head.contains(starPosDir) then Some(regions.head)
      else assign(regions.tail)(starPosDir)
    }

    stars
      .groupBy(assign(regions))
      .toList
      .flatMap((o, d) => o.map(k => (k, d)))
      .sortBy((r, _) => r.id)
  }

  def starsPerSectorEqualized(stars: Seq[StarPosDir], sectorCnt: Int): Seq[Seq[StarPosDir]] = {

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

    val sts = createSectors(sectorCnt)
    val starsGrouped = stars
      .groupBy(starToSector(sts))
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

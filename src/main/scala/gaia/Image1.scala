package gaia

import com.sun.imageio.plugins.common.BogusColorSpace
import gaia.Data.Star
import gaia.X3d.Shapable
import gaia.X3d.Shapable.Line

import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.Path._
import java.nio.file.{Files, Path}
import java.util.Base64
import scala.util.Random


object Image1 {

  import X3d._

  enum OutLocation {
    case Work, Models
  }

  case class ShellDef(
                       shellId: String,
                       startDist: Double,
                       endDist: Double,
                       starProb: Double,
                       starsToShapable: (stars: Iterable[Star]) => Iterable[Shapable],
                       transform: Star => Star = identity,
                     )

  def oneShellSpheres(id: String): Unit = {
    val starsToShapable = shapabelsStarsToSperes(0.02, Color.gray(1.0))(_)
    val min = 7.0
    val max = 9.0
    val starProb = 0.1
    val bgc = Color.darkBlue
    oneShell(id, bgc, min, max, starProb, starsToShapable, OutLocation.Work)
  }

  def oneShellPoints(id: String): Unit = {
    val starsToShapable = shapabelsStarsToPoints(Color.yellow)(_)
    val min = 7.0
    val max = 9.0
    val starProb = 1.0
    val bgc = Color.darkBlue
    oneShell(id, bgc, min, max, starProb, starsToShapable, OutLocation.Work)
  }

  def shellsSphere(id: String): Unit = {

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("05", 5.0, 5.2, 200 / 1000.0, shapabelsStarsToSperes(0.04, Color.darkRed)(_)),
      ShellDef("08", 8.0, 8.2, 800 / 1000.0, shapabelsStarsToSperes(0.06, Color.orange)(_)),
      ShellDef("11", 11.0, 11.2, 1000 / 1000.0, shapabelsStarsToSperes(0.08, Color.yellow)(_)),
    )
    val bgColor = Color.darkBlue

    multiShells(id, shells, bgColor)
  }

  def shellsPoints(id: String): Unit = {

    def equalDist(dist: Double)(star: Star): Star = star.copy(parallax = 1.0 / dist)

    val shapeSize = 0.02
    val shells = Seq(
      ShellDef("a", 5.0, 6.0, 400 / 1000.0, shapabelsStarsToPoints(Color.yellow)(_), equalDist(5.5)_),
      ShellDef("b", 8.0, 9.0, 700 / 1000.0, shapabelsStarsToPoints(Color.orange)(_), equalDist(8)_),
      ShellDef("c", 11.0, 12.0, 1000 / 1000.0, shapabelsStarsToPoints(Color.darkRed)(_), equalDist(10.5)_),
    )
    val bgColor = Color.darkBlue

    multiShells(id, shells, bgColor)
  }

  private def oneShell(id: String, bgColor: Color, min: Double, max: Double, starProb: Double,
                       starsToShapable: Iterable[Star] => Iterable[Shapable], outLocation: OutLocation) = {
    def draw(bgColor: Color): Seq[Shapable] = {

      val stars = filteredStars
        .filter(filterShell(min, max, starProb)(_))
      println(s"using ${stars.size} stars for image1 $id")
      val galacicCenter = Util.toVec(266.25, -28.94, 8)
      starsToShapable(stars).toSeq
      ++ shapablesCoordinates(2, bgColor)
      ++ shapablesCoordinates(4, bgColor, offset = galacicCenter)
    }

    val fnam = s"image1_$id.x3d"
    val imgFile = filePath(outLocation, fnam)
    X3d.drawTo(imgFile, draw, backColor = bgColor)
    println(s"Created image for $id at ${imgFile.toAbsolutePath}")
  }


  private def multiShells(id: String, shells: Seq[ShellDef], bgColor: Color) = {
    def draw(bgColor: Color): Seq[Shapable] = {
      val shapes = shells
        .flatMap { shellDef =>
          val stars = filteredStars
            .filter(filterShell(shellDef.startDist, shellDef.endDist, shellDef.starProb)(_))
            .map(shellDef.transform)
          println(s"Using ${stars.size} stars in shell ${shellDef.shellId}")
          shellDef.starsToShapable(stars)
        }
      val galacicCenter = Util.toVec(266.25, -28.94, 8)
      shapes
      ++ shapablesCoordinates(5, bgColor)
      ++ shapablesCoordinates(10, bgColor, offset = galacicCenter)
    }

    val imgFile = image1Dir.resolve(s"image1_$id.x3d")
    X3d.drawTo(imgFile, draw, backColor = bgColor)
    println(s"Created image for $id at $imgFile")
  }

  private def filteredStars: Seq[Star] = {

    def filterBasic(star: Star): Boolean = {
      if (star.parallax <= 0) return false
      if (Random.nextDouble() >= 0.2) return false
      true
    }

    val cacheFile = image1Dir.resolve(s"cache_image1.csv")
    starsFilteredAndCached(cacheFile, filterBasic)
  }

  private def filterShell(min: Double, max: Double, prob: Double)(star: Star): Boolean = {
    if (star.parallax <= 0) return false
    val dist = 1.0 / star.parallax
    if (dist < min || dist > max) return false
    if (Random.nextDouble() > prob) return false
    return true
  }

  private def shapabelsStarsToSperes(radius: Double, color: Color)(stars: Iterable[Star]): Iterable[Shapable] = {
    stars
      .map(toVec)
      .map(v => Shapable.Sphere(translation = v, color = color, radius = radius, solid = false))
  }

  private def shapabelsStarsToPoints(color: Color)(stars: Iterable[Star]): Iterable[Shapable] = {
    val vecs = stars.map(toVec)
    Seq(Shapable.PointSet(positions = vecs, color = color))
  }


  private def shapablesCoordinates(len: Double, bgColor: Color, offset: Vec = Vec.zero): Seq[Shapable] = {
    def ends = Seq(
      (Vec(1, 0, 0).mul(len).add(offset), Vec(-1, 0, 0).mul(len).add(offset), Color.gray(0.9)),
      (Vec(0, 1, 0).mul(len).add(offset), Vec(0, -1, 0).mul(len).add(offset), Color.gray(0.9)),
      (Vec(0, 0, 1).mul(len).add(offset), Vec(0, 0, -1).mul(len).add(offset), Color.gray(0.9)),
    )

    def coord(e1: Vec, e2: Vec, color: Color): Seq[Line] = {
      Seq(
        Shapable.Line(start = offset, end = e1, startColor = color, endColor = bgColor),
        Shapable.Line(start = offset, end = e2, startColor = color, endColor = bgColor),
      )
    }

    ends.flatMap {
      coord
    }
  }

  private def filePath(outLocation: OutLocation, fileName: String) = {
    val imgFile = outLocation match {
      case OutLocation.Work => image1Dir.resolve(fileName)
      case OutLocation.Models => of("src", "main", "html", "models", fileName)
    }
    imgFile
  }

  private def toVec(star: Star): Vec = {
    val dist = 1 / star.parallax
    val ra = star.ra
    val dec = star.dec
    Util.toVec(ra, dec, dist)
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

  lazy val image1Dir: Path = {
    val imagePath = Util.datapath.resolve("image1")
    if !Files.exists(imagePath)
      Files.createDirectories(imagePath)
    imagePath
  }
}

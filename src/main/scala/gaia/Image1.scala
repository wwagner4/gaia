package gaia

import com.sun.imageio.plugins.common.BogusColorSpace
import gaia.Data.Star
import gaia.X3d.Shapable
import gaia.X3d.Shapable.Line

import java.io._
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.file.{Files, Path}
import java.util.Base64


/*
Dataanalyse basic
ra: 0 - 360
dec: -90 +90
parallaxe: -51.55414944640443 / 304.21902198647064

explore parallaxe
histo: -60 -- 310. cnt / range / width. 20 / 370.0 / 18.5

questions / TODOs :
- how many parallaxes are negative ?
  71149 of 7183262 parallaxes are negative. This is 0.99 %
- Distance is from 0.003_28710 to 2_121_788.01009 kpc (kiloparsec)
- Diameter of the milkiway 30 kpc
- Distance of the sun from the galactic center 8.6 kpc
- How many starts are further away than 30 kpc ? 
- From 7112113 stars 37598 (0.53 %) are further away than 30 kpc
- what mean the meaned distance compared to the size of the milkiway and 
  relative to the location of the solarsystem in the milkiway?
- move these informations to readme.
 */


object Image1 {

  import X3d._

  def shell7kpc(id: String): Unit = {

    val shapeSize = 0.03
    val cacheFile = workDir.resolve(s"cache_${id}.csv")
    val imgFile = workDir.resolve(s"image1_$id.x3d")

    def draw(bgColor: Color): Seq[Shapable] = {
      def filterBasic(star: Star): Option[Star] = {
        if (star.parallax <= 0) return None
        val dist = 1.0 / star.parallax
        if (dist < 7 || dist > 7.02) return None
        return Some(star)
      }

      val galacicCenter = toVec(266.25, -28.94, 8)
      val stars = starsFiltered(cacheFile, filterBasic)
        .map(toVec)
        .map(v => Shapable.Sphere(translation = v, color = Color.gray(0.5), radius = shapeSize, solid = false))
      stars 
      ++ drawCoordinates(2, bgColor) 
      ++ drawCoordinates(2, bgColor, offset=galacicCenter)
    }

    Util.drawTo(imgFile, draw, backColor = Color.black)
    println(s"Created image for $id at $imgFile")

  }

  def drawCoordinates(len: Double, bgColor: Color, offset: Vec = Vec.zero): Seq[Shapable] = {
    def ends = Seq(
      (Vec(1, 0, 0).mul(len).add(offset), Vec(-1, 0, 0).mul(len).add(offset), Color.gray(0.3)),
      (Vec(0, 1, 0).mul(len).add(offset), Vec(0, -1, 0).mul(len).add(offset), Color.gray(0.3)),
      (Vec(0, 0, 1).mul(len).add(offset), Vec(0, 0, -1).mul(len).add(offset), Color.gray(0.3)),
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

  def toVec(star: Star): Vec = {
    val dist = 1 / star.parallax
    val ra = star.ra
    val dec = star.dec
    toVec(ra, dec, dist)
  }

  def toVec(ra: Double, dec: Double, dist: Double) = {
    val r = math.Pi / 180
    val x = math.cos(ra * r) * math.cos(dec * r) * dist
    val y = math.sin(ra * r) * math.cos(dec * r) * dist
    val z = math.sin(dec * r) * dist
    Vec(x, y, z)
  }

  private def starsFiltered(cache: Path, filterStar: Star => Option[Star]): Seq[Star] = {

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
        .flatMap(filterStar)
        .toSeq
      toCsv(stars)
      println(s"filtered basic to ${stars.size} stars")
      stars
    }
  }

  private def workDir: Path = {
    val imagePath = Util.datapath.resolve("image1")
    if !Files.exists(imagePath)
      Files.createDirectories(imagePath)
    imagePath
  }


}

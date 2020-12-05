package gaia

import com.sun.imageio.plugins.common.BogusColorSpace
import gaia.Data.Star
import gaia.X3d.Shapable
import gaia.X3d.Shapable.Line1

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

  def draw(): Unit = {
    quickDraw01()
  }

  def quickDraw01(): Unit = {
    println(s"drawing image1 quickDraw01")
    
    val id = "01"
    val boxSize = 0.05
    val starsFile = workDir.resolve(s"stars_${id}.ser")

    def draw(bgColor: Color): Seq[Shapable] = {
      def filterBasic(star: Star): Option[Star] = {
        if (star.parallax <= 0) return None
        val dist = 1.0 / star.parallax
        if (dist < 7 || dist > 7.02) return None
        return Some(star)
      }

      val stars = starsCached(starsFile, filterBasic, reload = false)
        .map(toVec)
        .map(v => Shapable.Box(translation = v, color = Color.orange, size = Vec(boxSize, boxSize, boxSize), solid = false))
      stars ++ drawCoordinates(7, bgColor)
    }

    val imgFile = workDir.resolve(s"image_$id.x3d")
    Util.drawTo(imgFile, draw, backColor = Color.black)

  }

  def drawCoordinates(len: Double, bgColor: Color): Seq[Shapable] = {
    def ends = Seq(
      (Vec(1, 0, 0), Vec(-1, 0, 0), Color.red),
      (Vec(0, 1, 0), Vec(0, -1, 0), Color.orange),
      (Vec(0, 0, 1), Vec(0, 0, -1), Color.yellow),
    )

    def coord(e1: Vec, e2: Vec, color: Color): Seq[Line1] = {
      Seq(
        Shapable.Line1(end = e1.mul(len), startColor = color, endColor = bgColor),
        Shapable.Line1(end = e2.mul(len), startColor = color, endColor = bgColor),
      )
    }

    ends.flatMap {
      coord
    }
  }

  def toVec(star: Star): Vec = {
    val dist = 1 / star.parallax
    val r = math.Pi / 180
    val x = math.cos(star.ra * r) * math.cos(star.dec * r) * dist
    val y = math.sin(star.ra * r) * math.cos(star.dec * r) *dist
    val z = math.sin(star.dec * r) * dist
    Vec(x, y, z)
  }

  private def starsCached(starsFile: Path, filterStar: Star => Option[Star], reload: Boolean): Seq[Star] = {
    println(f"stars file: $starsFile")

    def ser(value: Any): Unit = {
      def serialise(value: Any): String = {
        val stream: ByteArrayOutputStream = new ByteArrayOutputStream()
        val oos = new ObjectOutputStream(stream)
        oos.writeObject(value)
        oos.close
        new String(
          Base64.getEncoder().encode(stream.toByteArray),
          UTF_8
        )
      }

      val str = serialise(value)
      Files.writeString(starsFile, str)
    }

    def dser[T](clazz: Class[T]): T = {
      def deserialise(str: String): Any = {
        val bytes = Base64.getDecoder().decode(str.getBytes(UTF_8))
        val ois = new ObjectInputStream(new ByteArrayInputStream(bytes))
        val value = ois.readObject
        ois.close
        value
      }

      deserialise(Files.readString(starsFile)).asInstanceOf[T]
    }

    if (!reload && Files.exists(starsFile)) {
      val stars = dser(classOf[Seq[Data.Star]])
      println(s"filtered (cached) basic to ${stars.size} stars")
      stars
    }
    else {
      val stars = Data.readBasic
        .flatMap(filterStar)
        .toSeq
      ser(stars)
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

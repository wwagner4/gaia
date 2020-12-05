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
    println("Drawing image 1")
    quickDrawSomething()
  }

  def quickDrawSomething(): Unit = {
    def draw(bgColor: Color): Seq[Shapable] = {
      Seq(
        Shapable.Cylinder(translation = Vec(1, 0, 0), radius = 0.1, height = 0.1, color = Color.red),
        Shapable.Cylinder(translation = Vec(-1, 0, 0), radius = 0.1, height = 0.1, color = Color.red),
        Shapable.Cylinder(translation = Vec(0, 1, 0), radius = 0.1, height = 0.1, color = Color.orange),
        Shapable.Cylinder(translation = Vec(0, -1, 0), radius = 0.1, height = 0.1, color = Color.orange),
        Shapable.Cylinder(translation = Vec(0, 0, -1), radius = 0.1, height = 0.1, color = Color.yellow),
        Shapable.Cylinder(translation = Vec(0, 0, 1), radius = 0.1, height = 0.1, color = Color.yellow),
      )
      ++ drawCoordinates(1, bgColor)
    }

    val imgFile = workDir.resolve("Image1.x3d")
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

    ends.flatMap {coord}
  }

  def quickShowStars(): Unit = {
    val starsFile = workDir.resolve("stars_01.ser")
    starsCached(starsFile).foreach(println(_))
  }

  private def starsCached(starsFile: Path): Seq[Star] = {
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

    if (Files.exists(starsFile)) {
      dser(classOf[Seq[Data.Star]])
    }
    else {
      val stars = Data.readBasic
        .filter(_.parallax > 0.0)
        .map(s => (1.0 / s.parallax, s))
        .filter(s => s._1 > 8 && s._1 < 8.01)
        .map(_._2)
        .toSeq
      ser(stars)
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

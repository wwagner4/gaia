package gaia

import gaia.Data.Star

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

  def draw(): Unit = {
    println("Drawing image 1")

  }
  
  def quickShowStars(): Unit ={
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

import java.nio.file.{Files, Path}

import scala.io._
import scala.util.Random


object Main {


  def main(args: Array[String]): Unit = {
    val backColor = X3d.Color.darkBlue
    val id = "007"
    val outfileName = s"gaia_$id.x3d"
    val outfile = Util.outpath.resolve(outfileName)
    val shapables = Util.experimental02()
    val xml = X3d.createXml(shapables, outfileName, backColor)
    Util.writeString(outfile, xml)
  }
}

object Util {

  def ranOff(factor: Double): Double = (Random.nextDouble() - 0.5) * factor

  lazy val outpath: Path = {
    val outdir = System.getenv("OUTDIR")
    if (outdir == null) throw IllegalArgumentException("Environment variable OUTDIR must be defined")
    val result = Path.of(outdir)
    if (!Files.exists(result)) throw IllegalArgumentException(s"Directory ${result} must exist")
    result
  }

  def experimental01(backColor: X3d.Color): Seq[X3d.Shapable] = {
    def ranDist(shape: (X3d.Vec) => Seq[X3d.Shapable]): Seq[X3d.Shapable] = {
      (0 to 10)
        .flatMap { _ =>
          val off = X3d.Vec(ranOff(10), ranOff(10), ranOff(10))
          shape(off)
        }
    }

    def elem(off: X3d.Vec): Seq[X3d.Shapable] = {
      val c = X3d.Color.random
      val f = 15 + Random.nextDouble() * 30
      (1 to 200)
        .map(i => i / f)
        .map { t =>
          val scaling = (0.1 + t)
          val pos = X3d.Vec(math.sin(t) + off.x, math.cos(t) + off.y, math.cos(t) + off.z)
          X3d.Line(startColor = c, endColor = backColor, translation = pos, scaling = scaling)
        }
    }

    ranDist(elem)
  }

  def experimental02(): Seq[X3d.Shapable] = {
    
    import X3d._

    def elem(color: Color, off: Vec): Seq[X3d.Shapable] = {
      (1 to 10)
        .map(i => i * 1.0)
        .map { t =>
          val pos = Vec(off.x + t, off.y , off.z)
          val rot = Vec(0.1 * t, 0, 0)
          X3d.Cylinder(translation = pos, color = color, radius = 0.1, rotaion = rot)
        }

    }

    val offs = Seq(
      (Color.orange, Vec(0, 0, 0)),
      (Color.yellow, Vec(1, 0, 0)),
      (Color.red, Vec(0, 1, 0)),
      (Color.green, Vec(0, 0, 1)),
      (Color.blue, Vec(1, 1, 1)),
    )
    offs.flatMap{case (color, off) => elem(color, off)}
  }

  def writeString(outfile: Path, string: String): Unit =
    Files.writeString(outfile, string)
    println(s"wrote x3d to $outfile")


}

object X3d {

  case class Vec(x: Double, y: Double, z: Double) {
    def strComma = s"$x, $y, $z"
    def strNoComma = s"$x $y $z"
  }

  object Vec {
    def zero = Vec(0.0, 0.0, 0.0)
  }

  case class Color(r: Double, g: Double, b: Double) {
    def strNoComma = s"$r $g $b"
  }

  object Color {

    def random: Color = {
      val r = Random.nextDouble()
      val g = Random.nextDouble()
      val b = Random.nextDouble()
      Color(r, g, b)
    }

    def red = Color(1, 0, 0)

    def yellow = Color(1, 1, 0)

    def orange = Color(1, 0.5, 0)

    def green = Color(0, 1, 0)

    def blue = Color(0, 0, 1)

    def darkBlue = Color(0.1, 0.1, 0.2)

    def white = Color(0, 0, 0)

    def black = Color(1, 1, 1)

  }

  trait Shapable {
    def toShape: String
  }

  case class Cylinder(translation: Vec, color: Color, radius: Double = 1.0, rotaion: Vec = Vec.zero) extends Shapable {
    def toShape = {
      s"""
         |<Transform rotation='1 0 0 ${rotaion.x}'>
         |<Transform rotation='0 1 0 ${rotaion.y}'>
         |<Transform rotation='0 0 1 ${rotaion.z}'>
         |<Transform translation='${translation.strNoComma}'>
         |  <Shape>
         |     <Cylinder radius='$radius'/>
         |     <Appearance>
         |       <Material diffuseColor='${color.strNoComma}'/>
         |     </Appearance>
         |   </Shape>
         |</Transform>
         |</Transform>
         |</Transform>
         |</Transform>
         |""".stripMargin
    }
  }

  case class Line(startColor: Color = Color.white, endColor: Color = Color.yellow, translation: Vec = Vec.zero, scaling: Double = 1.0) extends Shapable {
    def toShape = {
      s"""
         |<Transform scale='${scaling}, 1, 1'>
         |   <Transform translation='${translation.strComma}'>
         |      <Shape>
         |         <IndexedLineSet colorIndex='0 1 -1' coordIndex='0 1 -1'>
         |            <Color color='${startColor.strNoComma} ${endColor.strNoComma}'/>
         |            <Coordinate point='0 0 0  1 0 0'/>
         |         </IndexedLineSet>
         |      </Shape>
         |   </Transform>
         |</Transform>
         |""".stripMargin
    }
  }

  def createXml(shapables: Seq[Shapable], title: String, backColor: Color): String = {

    val shapablesStr = shapables.map(_.toShape).mkString("\n")
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.3//EN" "https://www.web3d.org/specifications/x3d-3.3.dtd">
       |<X3D profile='Interchange' version='3.3' xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' xsd:noNamespaceSchemaLocation='https://www.web3d.org/specifications/x3d-3.3.xsd'>
       |  <head>
       |    <meta content='http://entelijan.net' name='reference'/>
       |  </head>
       |  <Scene>
       |    <WorldInfo title='$title'/>
       |    <Background skyColor='${backColor.strNoComma}'/>
       |    $shapablesStr
       |  </Scene>
       |</X3D>""".stripMargin
  }


}
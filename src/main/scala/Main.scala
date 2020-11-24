import java.nio.file.{Files, Path}

import scala.io._
import scala.util.Random


object Main {


  def main(args: Array[String]): Unit = {
    val backColor = X3d.Color.darkBlue
    val id = "010"
    val outfileName = s"gaia_$id.x3d"
    val outfile = Util.outpath.resolve(outfileName)
    val shapables = Util.drawLinesSimple()
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

  def drawFadingLines(backColor: X3d.Color): Seq[X3d.Shapable] = {

    import X3d._

    def ranDist(shape: (Vec) => Seq[Shapable]): Seq[Shapable] = {
      (0 to 10)
        .flatMap { _ =>
          val off = Vec(ranOff(0.1), ranOff(10), ranOff(10))
          shape(off)
        }
    }

    def lines(off: Vec): Seq[Shapable] = {
      val c = Color.white
      val f = 0.5
      (1 to 20)
        .map(i => i / f)
        .map { t =>
          val scaling = (0.1 + t)
          val pos = Vec(off.x, math.sin(t) + off.y, math.cos(t) + off.z)
          Line(startColor = c, endColor = backColor, translation = pos, scaling = 10)
        }
    }

    ranDist(lines)
  }

  def drawCylinderRotation(): Seq[X3d.Shapable] = {

    import X3d._

    def cylinders(color: Color, off: Vec): Seq[X3d.Shapable] = {
      (1 to 30)
        .map(i => i * 0.1)
        .map { t =>
          val pos = Vec(off.x + t, off.y, off.z)
          val rot = Vec(0.5 * t, 0, 0)
          X3d.Cylinder(translation = pos, color = color, radius = 0.01, height = 1.7, rotaion = rot)
        }
    }

    val offs = Seq(
      (Color.orange, Vec(0, 0, 0)),
      (Color.yellow, Vec(1, 0, 0)),
      (Color.red, Vec(0, 1, 0)),
      (Color.green, Vec(0, 0, 1)),
      (Color.blue, Vec(1, 1, 1)),
    )
    offs.flatMap { case (c, off) => cylinders(c, off) }
  }

  def drawLinesRotation(bgColor: X3d.Color): Seq[X3d.Shapable] = {

    import X3d._

    def lines(color: Color, off: Vec): Seq[X3d.Shapable] = {
      (0 to 500)
        .map(i => i * 0.1)
        .map { t =>
          val color = Color.random
          val pos = Vec(off.x, off.y, off.z)
          val rot = Vec(Util.ranOff(6.3), 0, Util.ranOff(6.3))
          X3d.Line(translation = pos, rotaion = rot, startColor = Color.white, endColor = bgColor, scaling = 20.0 + Util.ranOff(2))
        }

    }

    (0 to 20)
      .map {
        t =>
          val c = Color.random
          val offx = Util.ranOff(20)
          val offy = Util.ranOff(20)
          val offz = Util.ranOff(20)
          (c, Vec(offx, offy, offz))
      }
      .flatMap { case (color, off) => lines(color, off) }
  }

  def drawLinesSimple(): Seq[X3d.Shapable] = {

    import X3d._

    def lines(off: Vec): Seq[X3d.Shapable] = {
      (0 to 50)
        .map(i => i * 0.1)
        .map { t =>
          val pos = Vec(off.x, off.y + t, off.z)
          val rot = Vec(0, 0, 0)
          X3d.Line(translation = pos, rotaion = rot, startColor = Color.yellow, endColor = Color.orange,
            scaling = 20.0)
        }

    }

    val offs = Seq(
      Vec(0, 0, 0),
      Vec(0, 0, 1),
      Vec(0, 0, 2),
    )

    offs.flatMap(o => lines(o))
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

    def black = Color(0, 0, 0)

    def white = Color(1, 1, 1)

  }

  trait Shapable {
    def toShape: String
  }

  case class Cylinder(translation: Vec, color: Color, radius: Double = 1.0, height: Double = 1.0,
                      rotaion: Vec = Vec.zero) extends Shapable {
    def toShape = {
      val fromCenter = height / 2.0
      s"""
         |<Transform translation='${translation.strNoComma}'>
         |<Transform rotation='1 0 0 ${rotaion.x}' center='0, ${fromCenter}, 0'>
         |<Transform rotation='0 1 0 ${rotaion.y}' center='0, ${fromCenter}, 0'>
         |<Transform rotation='0 0 1 ${rotaion.z}' center='0, ${fromCenter}, 0'>
         |  <Shape>
         |     <Cylinder radius='$radius' height='$height'/>
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

  case class Line(startColor: Color = Color.white, endColor: Color = Color.yellow, translation: Vec = Vec.zero,
                  scaling: Double = 1.0, rotaion: Vec = Vec.zero) extends Shapable {
    def toShape = {
      s"""
         |<Transform scale='${scaling}, ${scaling}, ${scaling}'>
         |   <Transform translation='${translation.strComma}'>
         |   <Transform rotation='1 0 0 ${rotaion.x}' center='0, 0, 0'>
         |   <Transform rotation='0 1 0 ${rotaion.y}' center='0, 0, 0'>
         |   <Transform rotation='0 0 1 ${rotaion.z}' center='0, 0, 0'>
         |      <Shape>
         |         <IndexedLineSet colorIndex='0 1 -1' coordIndex='0 1 -1'>
         |            <Color color='${startColor.strNoComma} ${endColor.strNoComma}'/>
         |            <Coordinate point='0 0 0  1 0 0'/>
         |         </IndexedLineSet>
         |      </Shape>
         |   </Transform>
         |   </Transform>
         |   </Transform>
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
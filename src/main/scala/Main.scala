import java.nio.file.{Files, Path}


import scala.io._
import scala.util.Random


object Main {


  def main(args: Array[String]): Unit = {
    val backColor = X3d.Color.darkBlue
    val id = "006"
    val outfileName = s"gaia_$id.x3d"
    val outfile = Util.outpath.resolve(outfileName)
    val shapables = Util.randomDistribute01(Util.experimental01(backColor) _)
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

  def experimental01(backColor: X3d.Color)(xoff: Double, yoff: Double, zoff: Double): Seq[X3d.Shapable] =
    (0 to 200)
      .map(i => i / 50.0)
      .map { t =>
        val pos = X3d.Vec(math.sin(t) + xoff, math.cos(t) + yoff, zoff)
        X3d.Line(pos, X3d.Color.random, backColor)
      }


  def writeString(outfile: Path, string: String): Unit =
    Files.writeString(outfile, string)
    println(s"wrote x3d to $outfile")


  def randomDistribute01(shape: (Double, Double, Double) => Seq[X3d.Shapable]): Seq[X3d.Shapable] =
    (0 to 4)
      .flatMap { _ => shape(Util.ranOff(3), Util.ranOff(4), 0) }
}

object X3d {

  case class Vec(x: Double, y: Double, z: Double) {
    def display = s"$x, $y, $z"
  }

  case class Color(r: Double, g: Double, b: Double) {
    def display = s"$r $g $b"
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

  case class Cylinder(translation: Vec, color: Color) extends Shapable {
    def toShape = {
      s"""
         |<Transform translation='${translation.display}'>
         |   <Shape>
         |     <Cylinder radius='0.01'/>
         |     <Appearance>
         |       <Material diffuseColor='${color.display}'/>
         |     </Appearance>
         |   </Shape>
         |</Transform>
         |""".stripMargin
    }
  }

  case class Line(translation: Vec, startColor: Color, endColor: Color) extends Shapable {
    def toShape = {
      s"""
         |<Transform translation='${translation.display}'>
         |   <Shape>
         |   <IndexedLineSet colorIndex='0 1 -1' coordIndex='0 1 -1'>
         |      <Color color='${startColor.display} ${endColor.display}'/>
         |      <Coordinate point='0 0 0  1 0 0'/>
         |   </IndexedLineSet>
         |   </Shape>
         |</Transform>
         |""".stripMargin
    }
  }

  def createXml(shapables: Seq[Shapable], title: String, backColor: Color): String = {

    val theDisp = shapables.map(_.toShape).mkString("\n")
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.3//EN" "https://www.web3d.org/specifications/x3d-3.3.dtd">
       |<X3D profile='Interchange' version='3.3' xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' xsd:noNamespaceSchemaLocation='https://www.web3d.org/specifications/x3d-3.3.xsd'>
       |  <head>
       |    <meta content='http://entelijan.net' name='reference'/>
       |  </head>
       |  <Scene>
       |    <WorldInfo title='$title'/>
       |    <Background skyColor='${backColor.display}'/>
       |    $theDisp
       |  </Scene>
       |</X3D>""".stripMargin
  }


}
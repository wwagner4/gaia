package gaia

import scala.util.Random

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

    def darkRed = Color(0.5, 0, 0)

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

  object Shapable {

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
           |<Transform translation='${translation.strComma}'>
           |<Transform scale='${scaling}, ${scaling}, ${scaling}'>
           |<Transform rotation='1 0 0 ${rotaion.x}' center='0, 0, 0'>
           |<Transform rotation='0 1 0 ${rotaion.y}' center='0, 0, 0'>
           |<Transform rotation='0 0 1 ${rotaion.z}' center='0, 0, 0'>
           |      <Shape>
           |         <IndexedLineSet colorIndex='0 1 -1' coordIndex='0 1 -1'>
           |            <Color color='${startColor.strNoComma} ${endColor.strNoComma}'/>
           |            <Coordinate point='0 0 0  1 0 0'/>
           |         </IndexedLineSet>
           |      </Shape>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |""".stripMargin
      }
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

package gaia

import gaia.Data.Star

import java.nio.file.Path
import scala.util.Random

object X3d {

  import Vector._

  /**
   * Palettes generated by https://colorbrewer2.org/
   */
  enum Palette(brew: String) {

    case p1c10 extends Palette("['rgb(158,1,66)','rgb(213,62,79)','rgb(244,109,67)','rgb(253,174,97)','rgb(254,224,139)','rgb(230,245,152)','rgb(171,221,164)','rgb(102,194,165)','rgb(50,136,189)','rgb(94,79,162)']")

    case p1c5 extends Palette("['rgb(215,25,28)','rgb(253,174,97)','rgb(255,255,191)','rgb(171,217,233)','rgb(44,123,182)']")

    case p2c10 extends Palette("['rgb(165,0,38)','rgb(215,48,39)','rgb(244,109,67)','rgb(253,174,97)','rgb(254,224,144)','rgb(224,243,248)','rgb(171,217,233)','rgb(116,173,209)','rgb(69,117,180)','rgb(49,54,149)']")

    case p3c11 extends Palette("['rgb(64,0,75)','rgb(118,42,131)','rgb(153,112,171)','rgb(194,165,207)','rgb(231,212,232)','rgb(247,247,247)','rgb(217,240,211)','rgb(166,219,160)','rgb(90,174,97)','rgb(27,120,55)','rgb(0,68,27)']")

    case p3c10 extends Palette("['rgb(64,0,75)','rgb(118,42,131)','rgb(153,112,171)','rgb(194,165,207)','rgb(231,212,232)','rgb(217,240,211)','rgb(166,219,160)','rgb(90,174,97)','rgb(27,120,55)','rgb(0,68,27)']")

    case p3c5 extends Palette("['rgb(123,50,148)','rgb(194,165,207)','rgb(247,247,247)','rgb(166,219,160)','rgb(0,136,55)']")

    case p4c8 extends Palette("['rgb(255,153,153)','rgb(255,204,153)','rgb(255,255,253)','rgb(204,255,153)','rgb(153,255,153)','rgb(153,255,204)','rgb(153,255,255)','rgb(153,204,255)']")

    case p5c8 extends Palette("['rgb(255,51,51)','rgb(255,153,51)','rgb(255,255,51)','rgb(153,255,51)','rgb(51,155,51)','rgb(51,255,153)','rgb(51,255,255)','rgb(51,153,255)']")

    case p6c6 extends Palette("['rgb(102,255,0)','rgb(102,255,51)','rgb(102,255,102)','rgb(102,255,153)','rgb(102,255,204)','rgb(102,255,255)']")

    case p7c6 extends Palette("['rgb(204,102,0)','rgb(204,102,51)','rgb(204,102,102)','rgb(204,102,153)','rgb(204,102,204)','rgb(204,102,255)']")

    case p8c6 extends Palette("['rgb(255,255,0)','rgb(255,255,51)','rgb(255,255,102)','rgb(255,255,153)','rgb(255,255,204)','rgb(255,255,255)']")

    case p9c6 extends Palette("['rgb(204,102,0)','rgb(204,102,51)','rgb(204,102,102)','rgb(204,102,153)','rgb(204,102,204)','rgb(204,102,255)']")

    case p10c6 extends Palette("['rgb(255,204,0)','rgb(255,204,51)','rgb(255,204,102)','rgb(255,204,153)','rgb(255,204,204)','rgb(255,204,255)']")

    def colors: Seq[Color] = {
      val r = """rgb\((.*?)\)""".r
      val colorStrings = for (m <- r.findAllMatchIn(brew)) yield m.group(1)
      for (cs <- colorStrings.toSeq) yield {
        val vals = cs.split(",")
        val r = vals(0).toDouble / 256
        val g = vals(1).toDouble / 256
        val b = vals(2).toDouble / 256
        Color(r, g, b)
      }
    }

    def lazyColors: LazyList[Color] = {
      LazyList.continually(colors).flatten
    }
  }

  case class Color(r: Double, g: Double, b: Double) {
    def strNoComma = s"$r $g $b"

    def mul(factor: Double): Color = {
      def adj(v: Double) = math.min(math.max(0.0, v), 1.0)

      Color(adj(r * factor), adj(g * factor), adj(b * factor))
    }
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

    def veryDarkRed = Color(0.25, 0, 0)

    def yellow = Color(1, 1, 0)

    def orange = Color(1, 0.5, 0)

    def green = Color(0, 1, 0)

    def darkGreen = Color(0.01, 0.2, 0.01)

    def veryDarkGreen = Color(0.0001, 0.08, 0.0001)

    def blue = Color(0, 0, 1)

    def darkBlue = Color(0.1, 0.1, 0.2)

    def veryDarkBlue = Color(0.0001, 0.0001, 0.05)

    def black = Color(0, 0, 0)

    def white = Color(1, 1, 1)

    def gray(brightness: Double): Color = {
      val b = math.max(math.min(1.0, brightness), 0.0)
      Color(b, b, b)
    }

  }

  trait Shapable {
    def toShape: String
  }

  object Shapable {

    case class Cylinder(
                         position: Vec, direction: Vec = Vec(1, 0, 0),
                         radius: Double = 1.0, color: Color = Color.yellow) extends Shapable {
      def toShape = {
        val rotPol = direction.toPolarVec
        val ry = -rotPol.dec
        val rz = rotPol.ra
        val offset = Vec(0, -direction.length / 2.0, 0)
        s"""
           |<Transform translation='${position.strNoComma}'>
           |<Transform rotation='0 0 1 ${rz}' center='0, 0, 0'>
           |<Transform rotation='0 1 0 ${ry}' center='0, 0, 0'>
           |<Transform rotation='0 0 1 ${pidiv2}' center='0, 0, 0'>
           |<Transform translation='${offset.strNoComma}'>
           |   <Shape>
           |     <Cylinder radius='$radius' height='${direction.length}'/>
           |     <Appearance>
           |       <Material diffuseColor='${color.strNoComma}'/>
           |     </Appearance>
           |   </Shape>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |""".stripMargin
      }
    }

    case class Cone(
                     position: Vec, direction: Vec = Vec(1, 0, 0),
                     radius: Double = 1.0, height: Double = 1.0,
                     color: Color = Color.green) extends Shapable {
      def toShape = {
        val rotPol = direction.toPolarVec
        val ry = -rotPol.dec
        val rz = rotPol.ra
        val offset = Vec(0, -height / 2.0, 0)
        s"""
           |<Transform translation='${position.strNoComma}'>
           |<Transform rotation='0 0 1 ${rz}' center='0, 0, 0'>
           |<Transform rotation='0 1 0 ${ry}' center='0, 0, 0'>
           |<Transform rotation='0 0 1 ${pidiv2}' center='0, 0, 0'>
           |<Transform translation='${offset.strNoComma}'>
           |  <Shape>
           |     <Cone bottomRadius='$radius' height='$height'/>
           |     <Appearance>
           |       <Material diffuseColor='${color.strNoComma}'/>
           |     </Appearance>
           |   </Shape>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |""".stripMargin
      }
    }

    case class Box(position: Vec, rotaion: Vec = Vec.zero, color: Color = Color.orange, size: Vec = Vec(1, 1, 1),
                   solid: Boolean = true) extends Shapable {
      def toShape = {
        s"""
           |<Transform translation='${position.strNoComma}'>
           |<Transform rotation='1 0 0 ${rotaion.x}' center='0, 0, 0'>
           |<Transform rotation='0 1 0 ${rotaion.y}' center='0, 0, 0'>
           |<Transform rotation='0 0 1 ${rotaion.z}' center='0, 0, 0'>
           |  <Shape>
           |     <Box size='${size.strNoComma}' solid='$solid'/>
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

    case class PointSet(positions: Iterable[Vec], color: Color = Color.orange)
      extends Shapable {
      val points = positions.map(_.strNoComma).mkString(" ")
      val colors = points.map(_ => color.strNoComma).mkString(" ")

      def toShape = {
        s"""
           |<Shape>
           |<PointSet>
           |  <Color color='$colors' />
           |  <Coordinate point='$points'/>
           |  </PointSet>
           |</Shape>
           |""".stripMargin
      }
    }

    case class Sphere(position: Vec, color: Color = Color.orange, radius: Double = 1.0,
                      solid: Boolean = false) extends Shapable {
      def toShape = {
        s"""
           |<Transform translation='${position.strNoComma}'>
           |  <Shape>
           |     <Sphere radius='${radius}' solid='$solid'/>
           |     <Appearance>
           |       <Material diffuseColor='${color.strNoComma}'/>
           |     </Appearance>
           |   </Shape>
           |</Transform>
           |""".stripMargin
      }
    }

    case class Circle(translation: Vec, rotation: Vec = Vec.zero, color: Color = Color.orange, radius: Double = 1.0) extends Shapable {
      def toShape = {
        val points = Util.linearValues(0, 360, 200)
          .map(degToRad)
          .map(r => Vec(radius * math.sin(r), radius * math.cos(r), 0))
        val vertexCount = points.size
        val colorsStr = List.fill(vertexCount)(color).map(_.strNoComma).mkString("  ")
        val pointsStr = points.map(_.strNoComma).mkString("  ")
        s"""
           |<Transform translation='${translation.strNoComma}'>
           |<Transform rotation='1 0 0 ${rotation.x}' center='0, 0, 0'>
           |<Transform rotation='0 1 0 ${rotation.y}' center='0, 0, 0'>
           |<Transform rotation='0 0 1 ${rotation.z}' center='0, 0, 0'>
           |<Shape>
           |<LineSet vertexCount='$vertexCount'>
           |<Coordinate point='$pointsStr'/>
           |<Color color='$colorsStr'/>
           |</LineSet>
           |</Shape>
           |</Transform>
           |</Transform>
           |</Transform>
           |</Transform>
           |""".stripMargin
      }
    }

    case class Line(start: Vec = Vec.zero, end: Vec = Vec(1, 0, 0), startColor: Color = Color.white,
                    endColor: Color = Color.yellow) extends Shapable {
      def toShape = {
        s"""
           |<Shape>
           |<IndexedLineSet colorIndex='0 1 -1' coordIndex='0 1 -1'>
           |  <Color color='${startColor.strNoComma} ${endColor.strNoComma}'/>
           |  <Coordinate point='${start.strNoComma}  ${end.strNoComma}'/>
           |</IndexedLineSet>
           |</Shape>
           |""".stripMargin
      }
    }

  }

  def createXml(shapables: Seq[Shapable], title: String, backColor: Color, camera: Seq[Cam.Camera] = Seq.empty[Cam.Camera]): String = {

    val shapablesStr = shapables.map(_.toShape).mkString("\n")
    val viewpointStr = camera.map { c =>
      val rotPol = c.dir.mul(-1).toPolarVec
      val ry = -rotPol.dec
      val rz = rotPol.ra
      val ry1 = pidiv2
      s"""
         |<Transform translation='${c.pos.strNoComma}'>
         |<Transform rotation='0 0 1 ${rz}' center='0, 0, 0'>
         |<Transform rotation='0 1 0 ${ry}' center='0, 0, 0'>
         |<Transform rotation='0 1 0 ${ry1}' center='0, 0, 0'>
         |  <Viewpoint description='${c.name}' position='0 0 0'/>
         |</Transform>
         |</Transform>
         |</Transform>
         |</Transform>
         |""".stripMargin
    }.mkString("\n")

    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.3//EN" "https://www.web3d.org/specifications/x3d-3.3.dtd">
       |<X3D profile='Interchange' version='3.3' xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' xsd:noNamespaceSchemaLocation='https://www.web3d.org/specifications/x3d-3.3.xsd'>
       |  <head>
       |    <meta content='http://entelijan.net' name='reference'/>
       |  </head>
       |  <Scene>
       |    $viewpointStr
       |    <WorldInfo title='$title'/>
       |    <Background skyColor='${backColor.strNoComma}'/>
       |    $shapablesStr
       |  </Scene>
       |</X3D>""".stripMargin
  }

}

/*
        val rotPol = direction.toPolarVec
        val ry = -rotPol.dec
        val rz = rotPol.ra
        val offset = Vec(0, -height / 2.0, 0)
        s"""
           |<Transform translation='${position.strNoComma}'>
           |<Transform rotation='0 0 1 ${rz}' center='0, 0, 0'>
           |<Transform rotation='0 1 0 ${ry}' center='0, 0, 0'>
           |<Transform rotation='0 0 1 ${pidiv2}' center='0, 0, 0'>
           |<Transform translation='${offset.strNoComma}'>

 */
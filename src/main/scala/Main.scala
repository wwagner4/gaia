import java.nio.file.{Files, Path}

import scala.io._
import scala.util.Random


trait Shapable {
  def toShape: String
}

case class Position(x: Double, y: Double, z: Double)

case class Cylinder(pos: Position) extends Shapable {
  def toShape = {
    val r = Random.nextDouble()
    val g = Random.nextDouble()
    val b = Random.nextDouble()
    s"""
       |<Transform translation='${pos.x}, ${pos.y}, ${pos.z}'>
       |   <Shape>
       |     <Cylinder radius='0.005'/>
       |     <Appearance>
       |       <Material diffuseColor='$r $g $b'/>
       |     </Appearance>
       |   </Shape>
       |</Transform>
       |""".stripMargin
  }
}


object Main {


  def cylCyl(xoff: Double, yoff: Double) = (0 to 180)
    .map(i => i / 36.0)
    .map { t => Cylinder(Position(math.sin(t) + xoff, 0.0, math.cos(t) + yoff)) }

  def ranOff: Double = {
    Random.nextInt(300) / 30.0 - 15.0
  }

  val shapables = (0 to 25)
    .flatMap { _ => cylCyl(ranOff, ranOff) }

  val disp = shapables.map(x => x.toShape).mkString("\n")

  def createXml(title: String): String = {
    s"""<?xml version="1.0" encoding="UTF-8"?>
       |<!DOCTYPE X3D PUBLIC "ISO//Web3D//DTD X3D 3.3//EN" "https://www.web3d.org/specifications/x3d-3.3.dtd">
       |<X3D profile='Interchange' version='3.3' xmlns:xsd='http://www.w3.org/2001/XMLSchema-instance' xsd:noNamespaceSchemaLocation='https://www.web3d.org/specifications/x3d-3.3.xsd'>
       |  <head>
       |    <meta content='CylinderExample.x3d' name='title'/>
       |    <meta content='Cylinder geometric primitive node.' name='description'/>
       |    <meta content='Leonard Daly and Don Brutzman' name='creator'/>
       |    <meta content='1 January 2007' name='created'/>
       |    <meta content='14 June 2020' name='modified'/>
       |    <meta content='http://X3dGraphics.com' name='reference'/>
       |    <meta content='https://www.web3d.org/x3d/content/examples/X3dResources.html' name='reference'/>
       |    <meta content='Copyright Don Brutzman and Leonard Daly 2007' name='rights'/>
       |    <meta content='X3D book, X3D graphics, X3D-Edit, http://www.x3dGraphics.com' name='subject'/>
       |    <meta content='http://X3dGraphics.com/examples/X3dForWebAuthors/Chapter02GeometryPrimitives/CylinderExample.x3d' name='identifier'/>
       |    <meta content='X3D-Edit 3.3, https://savage.nps.edu/X3D-Edit' name='generator'/>
       |    <meta content='../license.html' name='license'/>
       |  </head>
       |  <Scene>
       |    <WorldInfo title='Cylinder.x3d'/>
       |    <Background skyColor='0.1 0.1 0.2'/>
       |    $disp
       |  </Scene>
       |</X3D>""".stripMargin
  }

  def writeXml(outfile: Path, xml: String): Unit = {
    Files.writeString(outfile, xml)
    println(s"wrote x3d to $outfile")
  }

  def main(args: Array[String]): Unit = {
    val id = "003"


    val outdir = System.getenv("OUTDIR")
    if outdir == null
      throw IllegalArgumentException("Environment variable OUTDIR must be defined")
    val outpath = Path.of(outdir)
    val outfileName = s"gaia_$id.x3d"
    val outfile = outpath.resolve(outfileName)
    val xml = createXml(outfileName)
    writeXml(outfile, xml)

  }


}

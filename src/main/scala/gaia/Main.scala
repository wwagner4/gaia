package gaia

import gaia.X3d.{Color, Shapable, Vec}

import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random


object Main {

  private case class GaiaDrawing(
                                  id: String,
                                  desc: String,
                                  drawingType: (id: String) => Unit
                                )

  private val drawings = Seq(
    GaiaDrawing("oss", "one shell around the galactic center with spheres", Image1.oneShellSpheres),
    GaiaDrawing("osp", "one shell around the galactic center with points", Image1.oneShellPoints),
    GaiaDrawing("shs", "multiple shells around the sun", Image1.shellsSphere),
    GaiaDrawing("shp", "multiple shells around the sun", Image1.shellsPoints),
    GaiaDrawing("sp2", "stars within a distance of 2 kpc to the sun", Image1.sphere2),
    GaiaDrawing("sp5", "stars within a distance of 5 kpc to the sun", Image1.sphere5),
    GaiaDrawing("sp8", "stars within a distance of 8 kpc to the sun", Image1.sphere8),
    GaiaDrawing("sp16", "stars within a distance of 16 kpc to the sun", Image1.sphere16),
    GaiaDrawing("ntsd27", "direction of stars within a distance of 27 pc to sun", Image1.nearSunDirections27pc),
    GaiaDrawing("ntsdvi", "direction and velocety of stars to a distace of 40 pc", Image1.nearSunVeloInner),
    GaiaDrawing("ntsdvo", "direction and velocety of stars of 45 pc distance", Image1.nearSunVeloOuter),
    GaiaDrawing("test", "run some test", gaia.X3d.test),
    GaiaDrawing("hp", "create homepage files and snipplets", gaia.Hp.createHp),
    GaiaDrawing("all", "all drawings", all),
  )
  
  private val ignoreIds = Seq("all", "hp", "test")
  
  private def all(id: String): Unit = {
    println(s"running $id")
    drawings
      .filter(gd => !ignoreIds.contains(gd.id))
      .toSeq
      .foreach { c =>
        println(s"running ${c.id}")
        c.drawingType(c.id)
      }
  }

  private def draw(drawable: (X3d.Color) => Seq[X3d.Shapable], drawableId: String, id: Option[String] = None,
                   backColor: X3d.Color = X3d.Color.darkBlue): Unit = {
    val id1 = id.getOrElse(java.util.UUID.randomUUID().toString)
    val outfileName = s"gaia-$drawableId-$id1.x3d"
    val outfile = Util.outpath.resolve(outfileName)
    X3d.drawTo(outfile, drawable, backColor)
  }

  private def findCall(callId: String): Option[GaiaDrawing] =
    drawings.map(c => (c.id, c)).toMap.get(callId)

  private def usage() = {
    val drawingIds = drawings.map(d => f"  ${d.id}%7s | ${d.desc}").mkString("\n")
    val msg =
      s"""
         |usage: <drawingId>
         |
         |drawingId ... Identifies a drawing.
         |
         |drawingId | description
         |----------|-----------------------------------------
         |$drawingIds
         |""".stripMargin
    println(msg)
  }

  def main(args: Array[String]): Unit = {
    val bgColor = Color.darkBlue
    if (args.length == 1) {
      findCall(args(0)).map(c => c.drawingType(c.id)).getOrElse(usage())
    } else
      usage()
  }


}



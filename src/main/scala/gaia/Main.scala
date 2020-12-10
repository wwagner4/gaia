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
    GaiaDrawing("dt", "data test", Data.dataTest),
    GaiaDrawing("all", "all drawings", all),
  )

  private def all(id: String): Unit = {
    println(s"running $id")
    drawings
      .filter(gd => gd.id != "dt" && gd.id != "all")
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



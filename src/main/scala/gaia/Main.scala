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
    GaiaDrawing("dt", "data test", Data.dataTest),
    GaiaDrawing("s7", "shell in the distance of 7 kpc", Image1.shell7kpc),
  )

  def main(args: Array[String]): Unit = {
    // TODO add outpath as argument
    val bgColor = Color.darkBlue
    if (args.length == 1) {
      findCall(args(0)).map(c => c.drawingType(c.id)).getOrElse(usage())
    } else
      usage()
  }

  private def draw(drawable: (X3d.Color) => Seq[X3d.Shapable], drawableId: String, id: Option[String] = None,
                   backColor: X3d.Color = X3d.Color.darkBlue): Unit = {
    val id1 = id.getOrElse(java.util.UUID.randomUUID().toString)
    val outfileName = s"gaia-$drawableId-$id1.x3d"
    val outfile = Util.outpath.resolve(outfileName)
    Util.drawTo(outfile, drawable, backColor)
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
}



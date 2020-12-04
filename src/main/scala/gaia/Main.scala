package gaia

import gaia.X3d.{Color, Shapable, Vec}

import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random


object Main {

  private enum DrawingType {

    case DRAWING(f: (X3d.Color) => Seq[X3d.Shapable])

    case CALLABLE(f: () => Unit)

  }

  private case class GaiaDrawing(
                          id: String,
                          desc: String,
                          drawingType: DrawingType
                        )

  private val drawings = Seq(
    GaiaDrawing("fl", "draw fading lines", DrawingType.DRAWING(Drawings.drawFadingLines)),
    GaiaDrawing("cr", "cylinders rotating", DrawingType.DRAWING(Drawings.drawCylinderRotation)),
    GaiaDrawing("lr", "lines rotating", DrawingType.DRAWING(Drawings.drawLinesRotation)),
    GaiaDrawing("lsr", "lines simple rotating", DrawingType.DRAWING(Drawings.drawLinesSimpleRot)),
    GaiaDrawing("lss", "lines simple scale", DrawingType.DRAWING(Drawings.drawLinesSimpleScale)),
    GaiaDrawing("dt", "data test", DrawingType.CALLABLE(Data.dataTest)),
    GaiaDrawing("i1", "image 1", DrawingType.CALLABLE(Image1.draw)),
  )

  def main(args: Array[String]): Unit = {
    // TODO add outpath as argument
    val bgColor = Color.darkBlue
    if (args.length == 1) {
      findCall(args(0)).map { c =>
        c.drawingType match
          case DrawingType.DRAWING(dr) => draw(dr, c.id, backColor = bgColor)
          case DrawingType.CALLABLE(ca) => ca()
      }.getOrElse(usage())
    }
    else if (args.length == 2) {
      findCall(args(0)).map { c =>
        c.drawingType match
          case DrawingType.DRAWING(dr) => draw(dr, c.id, id = Some(args(1)), backColor = bgColor)
          case DrawingType.CALLABLE(ca) => ca()
      }.getOrElse(usage())
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
         |usage: <drawingId> [<runId>]
         |
         |drawingId ... Identifies a drawing.
         |runId     ... Identifies the run. If not applied a UUID is used
         |
         |drawingId | description
         |----------|-----------------------------------------
         |$drawingIds
         |""".stripMargin
    println(msg)
  }
}



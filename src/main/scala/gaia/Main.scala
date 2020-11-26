package gaia

import java.nio.file.{Files, Path}

import gaia.X3d.{Color, Line, Shapable, Vec}

import scala.io._
import scala.util.Random


object Main {

  enum DrawingType {

    case DRAWING(f: (X3d.Color) => Seq[X3d.Shapable])

    case CALLABLE(f: () => Unit)

  }

  case class GaiaDrawing(
                          id: String,
                          desc: String,
                          drawingType: DrawingType
                        )

  val drawings = Seq(
    GaiaDrawing("fl", "draw fading lines", DrawingType.DRAWING(Drawings.drawFadingLines)),
    GaiaDrawing("cr", "cylinders rotating", DrawingType.DRAWING(Drawings.drawCylinderRotation)),
    GaiaDrawing("lr", "lines rotating", DrawingType.DRAWING(Drawings.drawLinesRotation)),
    GaiaDrawing("lsr", "lines simple rotating", DrawingType.DRAWING(Drawings.drawLinesSimpleRot)),
    GaiaDrawing("lss", "lines simple scale", DrawingType.DRAWING(Drawings.drawLinesSimpleScale)),
    GaiaDrawing("dt", "data test", DrawingType.CALLABLE(Data.readMeta)),
  )

  def main(args: Array[String]): Unit = {
    val bgColor = Color.darkBlue
    if (args.length == 1) {
      findCall(args(0)).map { c =>
        c.drawingType match
          case DrawingType.DRAWING(dr) => Util.draw(dr, c.id, backColor = bgColor)
          case DrawingType.CALLABLE(ca) => ca()
      }.getOrElse(usage())
    }
    else if (args.length == 2) {
      findCall(args(0)).map { c =>
        c.drawingType match
          case DrawingType.DRAWING(dr) => Util.draw(dr, c.id, id = Some(args(1)), backColor = bgColor)
          case DrawingType.CALLABLE(ca) => ca()
      }.getOrElse(usage())
    } else
      usage()
  }

  def findCall(callId: String): Option[GaiaDrawing] =
    drawings.map(c => (c.id, c)).toMap.get(callId)

  def usage() = {
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



package gaia

import java.nio.file.{Files, Path}

import gaia.X3d.{Color, Line, Shapable, Vec}

import scala.io._
import scala.util.Random


object Main {

  case class GaiaDrawing(
                   id: String,
                   desc: String,
                   drawing: (X3d.Color) => Seq[X3d.Shapable]
                 )

  val drawings = Seq(
    GaiaDrawing("fl", "draw fading lines", Drawings.drawFadingLines),
    GaiaDrawing("cr", "cylinders rotating", Drawings.drawCylinderRotation),
    GaiaDrawing("lr", "lines rotating", Drawings.drawLinesRotation),
    GaiaDrawing("lsr", "lines simple rotating", Drawings.drawLinesSimpleRot),
    GaiaDrawing("lss", "lines simple scale", Drawings.drawLinesSimpleScale),
  )

  def main(args: Array[String]): Unit = {
    val bgColor = X3d.Color.darkBlue
    if args.length == 1
      findCall(args(0)).map(c => Util.draw(c.drawing, c.id, backColor = bgColor)).getOrElse(usage())
    else if args.length == 2
      findCall(args(0)).map(c => Util.draw(c.drawing, c.id, id = Some(args(1)), backColor = bgColor)).getOrElse(usage())
    else
      usage()
  }

  def findCall(callId: String): Option[GaiaDrawing] =
    drawings.map(c => (c.id, c)).toMap.get(callId)

  def usage() = {
    val drawingIds = drawings.map(d => f"  ${d.id}%7s | ${d.desc}").mkString("\n")
    val msg = s"""
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



package gaia

import java.nio.file.{Files, Path}

import scala.util.Random

object Util {

  def draw(drawable: (X3d.Color) => Seq[X3d.Shapable], drawableId: String, id: Option[String] = None, backColor: X3d.Color = X3d.Color.darkBlue): Unit = {
    val id1 = id.getOrElse(java.util.UUID.randomUUID().toString)
    val outfileName = s"gaia-$drawableId-$id1.x3d"
    val outfile = Util.outpath.resolve(outfileName)
    val shapables = drawable(backColor)
    val xml = X3d.createXml(shapables, outfileName, backColor)
    Util.writeString(outfile, xml)
  }


  def ranOff(factor: Double): Double = (Random.nextDouble() - 0.5) * factor

  lazy val outpath: Path = {
    val outdir = System.getenv("OUTDIR")
    if (outdir == null) throw IllegalArgumentException("Environment variable OUTDIR must be defined")
    val result = Path.of(outdir)
    if (!Files.exists(result)) throw IllegalArgumentException(s"Directory ${result} must exist")
    result
  }


  def writeString(outfile: Path, string: String): Unit =
    Files.writeString(outfile, string)
    println(s"wrote x3d to $outfile")


}


package gaia

import java.nio.file.{Files, Path}

import scala.util.Random

object Util {

  def drawTo(outfile: Path, drawable: X3d.Color => Seq[X3d.Shapable], backColor: X3d.Color) = {
    val shapables = drawable(backColor)
    val xml = X3d.createXml(shapables, outfile.getFileName.toString, backColor)
    Util.writeString(outfile, xml)
  }

  def ranOff(factor: Double): Double = (Random.nextDouble() - 0.5) * factor

  def datapath: Path = {
    val home = Path.of(System.getProperty("user.home"))
    val result = home.resolve(Path.of("gaia", "data"))
    if !Files.exists(result)
      Files.createDirectories(result)
    result
  }

  def outpath: Path = {
    val result = datapath.resolve("out")
    if !Files.exists(result)
      Files.createDirectories(result)
    result
  }

  def writeString(outfile: Path, string: String): Unit =
    Files.writeString(outfile, string)
    println(s"wrote x3d to $outfile")


}


package gaia

import java.nio.file.{Files, Path}

import scala.util.Random

object Util {

  def draw(drawable: (X3d.Color) => Seq[X3d.Shapable], drawableId: String, id: Option[String] = None, 
           backColor: X3d.Color = X3d.Color.darkBlue, outPathStr: Option[String]): Unit = {
    val id1 = id.getOrElse(java.util.UUID.randomUUID().toString)
    val outfileName = s"gaia-$drawableId-$id1.x3d"
    val outfile = Util.outpath(outPathStr).resolve(outfileName)
    val shapables = drawable(backColor)
    val xml = X3d.createXml(shapables, outfileName, backColor)
    Util.writeString(outfile, xml)
  }


  def ranOff(factor: Double): Double = (Random.nextDouble() - 0.5) * factor

  def outpath(outPathStr: Option[String]): Path = {

    def defaultOutPath(): Path = {
      val home = Path.of(System.getProperty("user.home"))
      home.resolve(Path.of("gaia", "data", "out"))
    }

    val result = outPathStr.map(p => Path.of(p)).getOrElse(defaultOutPath())
    if !Files.exists(result)
      Files.createDirectories(result)
    result
  }


  def writeString(outfile: Path, string: String): Unit =
    Files.writeString(outfile, string)
    println(s"wrote x3d to $outfile")


}


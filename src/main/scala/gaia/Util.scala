package gaia

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import scala.util.Random
import scala.collection.JavaConverters._
import scala.language.implicitConversions

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


  def toCsv[T](datas: Iterable[T], f: T => Iterable[String], filePath: Path): Unit = {
    val bw = Files.newBufferedWriter(filePath)
    val pw = PrintWriter(bw)
    try {
      for (data <- datas) {
        val  line = f(data).mkString(",")
        pw.println(line)
      }
    } finally {
      bw.close
    }
  }
  def fromCsv[T](f: Array[String] => T, filePath: Path): Iterable[T] = {
    val br = Files.newBufferedReader(filePath)
    try {
      br.lines().iterator.asScala.map(line => f(line.split(","))).to(Iterable)
    } finally {
      br.close()
    }
  }
}


package gaia

import java.net.URL
import java.nio.file.Files
import java.util.zip.GZIPInputStream

object Data {

  case class FileNameMd5(
                          md5sum: String,
                          fname: String,
                        )

  case class Star(
                   ra: Double, // 5
                   dec: Double, // 7
                   parallax: Double, // 9
                 )

  def readMeta(): Unit = {
    stars()
  }

  def header(): Unit = {
    val fn = "GaiaSource_1000172165251650944_1000424567594791808.csv.gz"
    val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/$fn"
    println(urlStr)
    val header = scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
      .getLines()
      .take(1)
      .toSeq
    header(0)
      .split(",")
      .zipWithIndex
      .foreach { case (nam, i) => println("%4d - %s".format(i, nam)) }
  }

  def toStar(a: Array[String]): Option[Star] = {
    try {
      val p = a(9).toDouble
      if (p < 0) return None 
      Some(Star(a(5).toDouble, a(7).toDouble, p))
    } catch {
      case _: Exception => None
    }
  }
  
  def stars(): Unit = {
    allLines()
      .map(_.split(","))
      .flatMap(toStar)
      .zipWithIndex
      .foreach { case (star, i) => println("%10d - %.14f %.14f %.16f".format(i, star.ra, star.dec, star.parallax)) }
  }

  private def topLines(): Seq[String] = {
    val fn = "GaiaSource_1000172165251650944_1000424567594791808.csv.gz"
    val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/$fn"
    println(urlStr)
    scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
      .getLines()
      .filter(!_.startsWith("solu"))
      .toSeq
  }

  def allLines(): Iterator[String] = {
    readFilnameMd5.flatMap(lines(_))
  }

  def lines(fn: FileNameMd5): Iterator[String] = {
    val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/${fn.fname}"
    println(urlStr)
    scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
      .getLines()
      .filter(!_.startsWith("solu"))
  }

  def readFilnameMd5: Iterator[FileNameMd5] = {
    val is = this.getClass.getClassLoader.getResourceAsStream("MD5SUM.txt")
    scala.io.Source.fromInputStream(is)
      .getLines()
      .take(1000)
      .map(line => line.split("\\s+"))
      .map(a => FileNameMd5(a(0).trim, a(1).trim))
      .filter(_.fname.startsWith("GaiaSource"))
  }
}

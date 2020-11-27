package gaia

import java.io.{BufferedOutputStream, BufferedWriter, FileOutputStream, OutputStreamWriter, PrintWriter}
import java.net.URL
import java.nio.file.Files
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

object Data {

  case class FileNameMd5(
                          md5sum: String,
                          fname: String,
                        )

  case class Star(
                   ra: Double, // 5
                   dec: Double, // 7
                   parallax: Double, // 9
                   pmra: Double, // 12
                   pmdec: Double, // 14
                   radialVelocity: Double // 66
                 )

  def dataTest(): Unit = {
    writeIter(starsIter)
  }

  def writeIter(iter: Iterator[String]): Unit = {
    val dataFile = Util.outpath.resolve("data_source_all.csv.gz")
    val os = FileOutputStream(dataFile.toFile)
    val go = GZIPOutputStream(os)
    val osw = OutputStreamWriter(go)
    val bw = BufferedWriter(osw)
    val pw = PrintWriter(bw)
    try {
      iter.foreach(pw.println(_))
      println(s"wrote to ${dataFile.toAbsolutePath}")
    } finally {
      pw.close()
    }
  }

  def starsIter: Iterator[String] = {
    allLines()
      .map(_.split(","))
      .flatMap(toStar)
      .map { star => "%.14f,%.14f,%.16f,%.16f,%.16f,%.16f".format(star.ra, star.dec, star.parallax, star.pmdec, star.pmra, star.radialVelocity) }
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
      if (p < 0) {
        // println("parallaxe < 0")
        None
      }
      Some(Star(a(5).toDouble, a(7).toDouble, p, a(12).toDouble, a(14).toDouble, a(66).toDouble))
    } catch {
      case ex: Exception => 
        // println(s"Exception ${ex.getMessage}")
        None
    }
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

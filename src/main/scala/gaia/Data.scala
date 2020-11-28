package gaia

import java.io._
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

object Data {

  case class FileNameMd5(
                          md5sum: String,
                          fname: String,
                        )

  case class FileGroup(
                        id: Int,
                        fileNames: Seq[FileNameMd5]
                      ) {
    override def toString: String = s"FileGroup(id:$id fileNames:${fileNames.size})"
  }

  case class Star(
                   ra: Double, // 5
                   dec: Double, // 7
                   parallax: Double, // 9
                   pmra: Double, // 12
                   pmdec: Double, // 14
                   radialVelocity: Double // 66
                 )

  def dataTest(): Unit = {
    downloadGrouped(600)
  }

  def downloadGrouped(groupSize: Int): Unit = {
    val nams = readFilnameMd5.toList
    val groups: Seq[FileGroup] = nams.grouped(groupSize)
      .zipWithIndex
      .toSeq
      .map { case (nams, index) => FileGroup(index + 1, nams) }
    val gsize = groups.size
    groups.foreach { grp =>
      val dataFile = Util.outpath(None).resolve(s"gaia_source_group_${grp.id}_of_$gsize.csv.gz")
      //writeIter(outLineIter(nams.iterator), dataFile)
      println(s"writing $dataFile of group $grp")
    }

  }

  def writeIter(iter: Iterator[String], dataFile: Path): Unit = {
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

  def outLineIter(filenames: Iterator[FileNameMd5]): Iterator[String] = {
    allLines(filenames)
      .map(_.split(","))
      .flatMap(toStarZero)
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

  def zeroStar: Star = {
    Star(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)
  }

  def toStarFilter(a: Array[String]): Option[Star] = {
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

  def toStarZero(a: Array[String]): Option[Star] = {
    try {
      val p = a(9).toDouble
      if (p < 0) {
        // println("parallaxe < 0")
        Some(zeroStar)
      }
      Some(Star(a(5).toDouble, a(7).toDouble, p, a(12).toDouble, a(14).toDouble, a(66).toDouble))
    } catch {
      case ex: Exception =>
        // println(s"Exception ${ex.getMessage}")
        Some(zeroStar)
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

  def allLines(filenames: Iterator[FileNameMd5]): Iterator[String] = {
    filenames.flatMap(lines(_))
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
      .map(line => line.split("\\s+"))
      .map(a => FileNameMd5(a(0).trim, a(1).trim))
      .filter(_.fname.startsWith("GaiaSource"))
  }
}

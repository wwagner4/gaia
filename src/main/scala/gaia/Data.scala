package gaia

import java.io._
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.zip.{GZIPInputStream, GZIPOutputStream}

import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Data {

  sealed trait DataResource

  object DataResource {

    case class Http(url: URL) extends DataResource

    case class File(path: Path) extends DataResource

  }

  case class FileNameMd5(
                          md5sum: String,
                          fname: String,
                        )

  case class FileGroup(
                        id: Int,
                        fileNames: Seq[DataResource]
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

  case class DownloadConfig(
                             groupCount: Int,
                             filename: (Int, Int) => String,
                             regex: Int => String
                           )

  val downloadConfigs = Seq(
    ("BASIC", DownloadConfig(
      groupCount = 200,
      filename = (nr, nrCnt) => s"gaia_source_group_${nr}_of_$nrCnt.csv.gz",
      regex = nrCnt => s"""gaia.*group_(.*)_of_$nrCnt.*""",
    )),
  ).toMap

  def dataTest(id: String): Unit = {
    println(s"running $id")
  }

  def quickDownloadBasic(workPath: Path): Unit = downloadGrouped(downloadConfigs("BASIC"), Util.outpath(workPath))

  def readBasic(workPath: Path): Iterator[Star] = {

    def extractId(filePath: Path): Option[(Int, Path)] = {
      val fnamRegex = """.*gaia.*group_(.*)_of.*""".r
      filePath.toString match {
        case fnamRegex(id) => Some((id.toInt, filePath))
        case _ => None
      }
    }

    val basicPath = Util.datapath(workPath).resolve("basic")
    require(Files.exists(basicPath), s"Directory for basic data does not exist. $basicPath")

    Files.list(basicPath)
      .iterator
      .asScala
      .toSeq
      .flatMap(extractId)
      .sortBy { (nr, path) => nr }
      .map { (_, path) => path }
      .map(DataResource.File(_))
      .iterator
      .flatMap(readLines)
      .map(_.split(","))
      .map(toStarBasic)
  }

  def downloadGrouped(downladConfig: DownloadConfig, outpath: Path): Unit = {

    val groupCount = downladConfig.groupCount

    def readyIds: Seq[Int] = {
      def toId(path: Path): Option[Int] = {
        val fnamStr = downladConfig.regex(groupCount)
        val fnamRegex = fnamStr.r
        val theName = path.getFileName.toString
        theName match {
          case fnamRegex(id) => Some(id.toInt)
          case _ => None
        }
      }

      Files.list(outpath)
        .iterator.asScala
        .flatMap(p => toId(p))
        .toSeq
    }

    val rids = readyIds
    val nams = readFilnameMd5
      .map { fn =>
        val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/${fn.fname}"
        DataResource.Http(URL(urlStr))
      }
      .toSeq
    val groupSize = (nams.size.toDouble / groupCount).ceil.toInt
    nams
      .grouped(groupSize)
      .zipWithIndex
      .toSeq
      .map { (gnams, index) => FileGroup(index + 1, gnams) }
      .filter(g => !rids.contains(g.id))
      .foreach { grp =>
        val dataFile = outpath.resolve(downladConfig.filename(grp.id, groupCount))
        val workFile = outpath.resolve(s"gaia_work.csv.gz")
        println(s"writing $groupSize files to $workFile of group $grp")
        val stars = grp.fileNames.iterator
          .flatMap(readLines)
          .map(_.split(","))
          .flatMap(toStarFilter)
        val newLines = stars.map(fromStar)
        writeIter(newLines, workFile)
        Files.move(workFile, dataFile)
        println(s"copied $workFile to $dataFile of group $grp")
      }

  }

  def fromStar(star: Star): String = {
    "%.14f,%.14f,%.16f,%.16f,%.16f,%.16f".format(
      star.ra, star.dec, star.parallax, star.pmdec, star.pmra, star.radialVelocity)
  }

  def readLines(data: DataResource): Iterator[String] = {
    data match {
      case DataResource.Http(url) =>
        // val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/${fn.fname}"
        println(s"downloading from $url")
        scala.io.Source.fromInputStream(GZIPInputStream(url.openStream()))
          .getLines()
          .filter(!_.startsWith("solu"))
      case DataResource.File(path) =>
        scala.io.Source.fromInputStream(GZIPInputStream(FileInputStream(path.toFile)))
          .getLines()
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

  def toStarBasic(a: Array[String]): Star = {
    Star(a(0).toDouble, a(1).toDouble, a(2).toDouble, a(3).toDouble, a(4).toDouble, a(5).toDouble)
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

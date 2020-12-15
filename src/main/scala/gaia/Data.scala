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

  private def quickStarsPerShell = {
    val shells = Seq(
      (" 1 kpc", 1.0, 1.1),
      (" 2 kpc", 2.0, 2.1),
      (" 3 kpc", 3.0, 3.1),
      (" 4 kpc", 4.0, 4.1),
      (" 5 kpc", 5.0, 5.1),
      (" 6 kpc", 6.0, 6.1),
      (" 7 kpc", 7.0, 7.1),
      (" 8 kpc", 8.0, 8.1),
      (" 9 kpc", 9.0, 9.1),
      ("10 kpc", 10.0, 10.1),
      ("11 kpc", 11.0, 11.1),
      ("12 kpc", 12.0, 12.1),
      ("13 kpc", 13.0, 13.1),
      ("14 kpc", 14.0, 14.1),
    )

    def filterBasic(star: Star)(min: Double, max: Double): Option[Star] = {
      if (star.parallax <= 0) return None
      val dist = 1.0 / star.parallax
      if (dist < min || dist > max) return None
      return Some(star)
    }

    for ((id, min, max) <- shells) yield {
      val cnt = readBasic.flatMap(filterBasic(_)(min, max)).size
      println(s"$id - $cnt stars")
    }
  }

  // Quickstart configurations
  def quickNegativeParallax = {
    var cntAll = 0
    var cntNegPar = 0
    var min = Double.MaxValue
    var max = Double.MinValue
    readBasic.foreach(s => {
      if (s.parallax <= 0) {
        cntNegPar += 1
        if (s.parallax < min) min = s.parallax
        if (s.parallax > max) max = s.parallax
      }
      cntAll += 1
    })
    val minDist = -1 / max
    val maxDist = -1 / min
    val perc = 100.0 * cntNegPar / cntAll
    println("=== negative parallax ===")
    println(f" $cntNegPar of $cntAll are negative parallaxes. $perc%.2f.")
    println(f" min parallax:  $min%20.2f            max parallax: $max%20.2f")
    println(f" max dist:      $minDist%20.3f kpc    min dist:     $maxDist%.3f kpc")
  }

  def quickDownloadBasic: Unit = downloadGrouped(downloadConfigs("BASIC"), Util.outpath)

  def quickHeader: Unit = header()

  def quickBasicSize: Unit = {
    val size = readBasic.size
    println(s"Size of basic is $size")
  }

  def quickBasicTop: Unit = readBasic.take(20).foreach(println(_))

  def readBasic: Iterator[Star] = {

    def extractId(filePath: Path): Option[(Int, Path)] = {
      val fnamRegex = """.*gaia.*group_(.*)_of.*""".r
      filePath.toString match {
        case fnamRegex(id) => Some((id.toInt, filePath))
        case _ => None
      }
    }

    val basicPath = Util.datapath.resolve("basic")
    require(Files.exists(basicPath), s"Directory for basic data does not exist. $basicPath")

    Files.list(basicPath)
      .iterator
      .asScala
      .toSeq
      .flatMap(extractId)
      .sortBy { case (nr, path) => nr }
      .map { case (_, path) => path }
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
      .map { case (gnams, index) => FileGroup(index + 1, gnams) }
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

  def header(): Unit = {
    val fn = "GaiaSource_1000172165251650944_1000424567594791808.csv.gz"
    val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/$fn"
    println(s"downloading from $urlStr")
    val header = scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
      .getLines()
      .take(1)
      .toSeq
    header(0)
      .split(",")
      .zipWithIndex
      .foreach { case (nam, i) => println("%4d - %s".format(i, nam)) }
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

  def toStarZero(a: Array[String]): Option[Star] = {

    val zeroStar = Star(0.0, 0.0, 0.0, 0.0, 0.0, 0.0)

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

  def readFilnameMd5: Iterator[FileNameMd5] = {
    val is = this.getClass.getClassLoader.getResourceAsStream("MD5SUM.txt")
    scala.io.Source.fromInputStream(is)
      .getLines()
      .map(line => line.split("\\s+"))
      .map(a => FileNameMd5(a(0).trim, a(1).trim))
      .filter(_.fname.startsWith("GaiaSource"))
  }
}

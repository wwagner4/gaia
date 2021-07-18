package gaia

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader, PrintWriter}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors, Future}
import java.util.{Locale, stream, Comparator}
import scala.jdk.StreamConverters._
import scala.jdk.CollectionConverters._
import scala.util.Random

object Util {

  import X3d.Color
  import Vector._
  import Gaia._


  def ranOff(factor: Double): Double = (Random.nextDouble() - 0.5) * factor

  def datapath(workPath: Path): Path = {
    val dataPath = workPath.resolve("data")
    if Files.notExists(dataPath) then Files.createDirectories(dataPath)
    dataPath
  }

  def outpath(workPath: Path): Path = {
    val result = datapath(workPath).resolve("out")
    if !Files.exists(result) then
      Files.createDirectories(result)
    result
  }

  def writeString(outfile: Path, string: String): Unit =
    Files.writeString(outfile, string)


  def toCsv[T](datas: Iterable[T], f: T => Iterable[String], filePath: Path): Unit = {
    val bw = Files.newBufferedWriter(filePath)
    val pw = PrintWriter(bw)
    try {
      for (data <- datas) {
        val line = f(data).mkString(",")
        pw.println(line)
      }
    } finally {
      bw.close()
    }
  }

  def fromCsv[T](f: Array[String] => T, filePath: Path): Iterable[T] = {
    val br = Files.newBufferedReader(filePath)
    try {
      br.lines().toScala(List).map(line => f(line.split(","))).to(Iterable)
    } finally {
      br.close()
    }
  }

  def fromFile(filePath: Path): String = {
    val br = Files.newBufferedReader(filePath)
    try {
      br.lines().toScala(Seq).mkString("")
    } finally {
      br.close()
    }
  }

  def fromResource(resName: String): String = {
    val url = this.getClass.getClassLoader.getResource(resName)
    if url == null then throw IllegalArgumentException(s"Unknown resource '$resName'")
    scala.io.Source.fromInputStream(url.openStream()).mkString
  }

  private def xValues(cnt: Int): Seq[Double] = {
    require(cnt >= 2, s"cnt:$cnt must be greater equal 2")
    val step = 1.0 / (cnt - 1)
    (0 until cnt)
      .map(i => i * step)
  }

  private def linearFunction(startValue: Double, endValue: Double): Double => Double = {
    def lin(a: Double, k: Double)(x: Double): Double = a + k * x

    val k = endValue - startValue
    lin(startValue, k)(_)
  }

  private def squaredFunction(startValue: Double, endValue: Double): Double => Double = {
    def lin(a: Double, k: Double)(x: Double): Double = a + k * x * x

    val k = endValue - startValue
    lin(startValue, k)(_)
  }

  def colorTransition(startColor: Color, endColor: Color, cnt: Int): Seq[Color] = {
    val fr = linearFunction(startColor.r, endColor.r)
    val fg = linearFunction(startColor.g, endColor.g)
    val fb = linearFunction(startColor.b, endColor.b)
    xValues(cnt).map(x => Color(fr(x), fg(x), fb(x)))
  }

  def linearValues(start: Double, end: Double, cnt: Int): Seq[Double] = {
    val f = linearFunction(start, end)
    xValues(cnt).map(f)
  }

  def squaredValues(start: Double, end: Double, cnt: Int): Seq[Double] = {
    val f = squaredFunction(start, end)
    xValues(cnt).map(f)
  }

  def recursiveCopy(sourceDir: Path, destinationDir: Path): Unit = {
    if (Files.notExists(destinationDir)) Files.createDirectories(destinationDir)
    Files.walk(sourceDir).forEach((sourcePath: Path) => {
      def foo(sourcePath: Path) = try {
        val targetPath = destinationDir.resolve(sourceDir.relativize(sourcePath))
        printf("Copying %s to %s%n", sourcePath, targetPath)
        Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING)
      } catch {
        case ex: IOException => printf(s"I/O error: $ex \n")
      }

      foo(sourcePath)
    })
  }

  def runAllCommands(cmds: Iterable[Iterable[String]]): Unit = {

    val allCnt = cmds.size

    class StreamGobbler(val name: String, val inputStream: InputStream) extends Runnable {
      private def handleInputStream(in: InputStream): Unit = {

        def handle(cnt: Int): Unit = {
          val br = new BufferedReader(new InputStreamReader(in))
          try {
            br.lines().forEach { line =>
              val msg = if cnt > 0 then s"ERROR occurred $cnt $name - $line" else s"$name - $line"
              println(msg)
            }
            br.close()
          } catch {
            case e: Exception =>
              try {
                println(s"ERROR ${cnt} reading process $name stream")
                e.printStackTrace
                br.close()
              } finally {
                handle(cnt + 1)
              }
          }
        }

        handle(0)
      }

      override def run(): Unit = {
        handleInputStream(inputStream)
      }

    }

    val gobbleExec: ExecutorService = Executors.newFixedThreadPool(2)
    val procExec: ExecutorService = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    try {
      def start(cmd: List[String], cnt: Int): Int = {
        val process = new ProcessBuilder()
          .command(cmd.asJava)
          .start()

        val inputGobbler = StreamGobbler("", process.getInputStream())
        gobbleExec.submit(inputGobbler)
        val errorGobbler = StreamGobbler("", process.getErrorStream())
        gobbleExec.submit(errorGobbler)
        println(s"started command $cnt of $allCnt - " + cmd.mkString(" "))
        val procResult = process.waitFor()
        println(s"finished command $cnt of $allCnt - " + cmd.mkString(" ") + " " + procResult)
        procResult
      }

      val futures = for ((cmd, i) <- cmds.zipWithIndex) yield {
        Thread.sleep(500)
        procExec.submit(() => start(cmd.toList, i + 1))
      }
      var states = futures.map(f => f.isDone)
      val sleepTimeMillis = 1000
      var timeMillis = 0
      while (!states.forall(s => s)) {
        Thread.sleep(sleepTimeMillis)
        states = futures.map(f => f.isDone)
        timeMillis += sleepTimeMillis
      }
      val exits = futures.map(f => f.get())
      println(s"finished all commands. Exit values: ${exits.mkString(",")}")
      val errros = exits.filter(_ != 0)
      if errros.nonEmpty then throw IllegalStateException("At least one of the processes finiched with error")
    } finally {
      gobbleExec.shutdownNow()
      procExec.shutdownNow()
    }
  }

  def intervals(number: Int, from: Double, to: Double): List[(Double, Double)] = {
    val diff = (to - from) / number

    def i(state: Double, result: List[(Double, Double)]): List[(Double, Double)] = {
      if state > to then result
      else {
        val end = state + diff
        if end >= to then (state, to) :: result
        else i(end, (state, end) :: result)
      }
    }

    i(from, List.empty[(Double, Double)]).reverse
  }

  def angle2DDeg(x: Double, y: Double): Int = {
    val a = radToDeg(math.asin(y / math.sqrt(x * x + y * y)))
    val a1 = if x < 0 then 180 - a else a
    val a2 = if a1 < 0 then a1 + 360.0 else a1
    val a3 = math.round(a2).toInt
    if a3 == 360 then 0 else a3
  }

  def runWithTmpdir(workDir: Option[Path] = None)(f: (tmpDir: Path) => Unit): Unit = {
    def runWithTmpdir3(f: (tmpDir: Path) => Unit): Unit = {
      val tmpWorkDir = Files.createTempDirectory("gaia")
      println(s"created tmp dir: $tmpWorkDir")
      try {
        f(tmpWorkDir)
      } finally {
        deleteDirRecursive(tmpWorkDir)
      }
    }

    def runWithTmpdir1(workDir: Path)(f: (tmpDir: Path) => Unit): Unit = {
      val tmpWorkDir = fileDirInOutDir(workDir, "tmp")
      deleteDirContentRecursive(tmpWorkDir)
      println(s"created tmp dir: $tmpWorkDir")
      f(tmpWorkDir)
    }

    workDir.map(wd => runWithTmpdir1(wd)(f)).getOrElse(runWithTmpdir3(f))
  }

  def deleteDirRecursive(dir: Path): Unit = {
    Files.walk(dir)
      .sorted(Comparator.reverseOrder)
      .forEach(f => Files.delete(f))
    println(s"deleted dir: $dir")
  }

  def deleteDirContentRecursive(dir: Path): Unit = {
    deleteDirRecursive(dir)
    Files.createDirectories(dir)
    println(s"deleted contents of dir: $dir")
  }

  def inDocker: Boolean = {
    val v = System.getenv("GAIA_IN_DOCKER")
    v != null && v == "YES"
  }

  def fileDirInOutDir(workDir: Path, dirName: String): Path = {
    val od = fileDirFromDir(workDir, "out")
    fileDirFromDir(od, dirName)
  }

  def fileDirFromDir(dir: Path, dirName: String): Path = {
    val theDir = dir.resolve(dirName)
    if Files.notExists(theDir) then Files.createDirectories(theDir)
    theDir
  }

  def fileCopy(file: Path, dir: Path): Unit = {
    Files.copy(file, dir.resolve(file.getFileName), StandardCopyOption.REPLACE_EXISTING)
  }

  def fileImageSize(fileImage: Path): (Int, Int) = {
    import javax.imageio.ImageIO
    import java.awt.image.BufferedImage
    val bimg = ImageIO.read(fileImage.toFile)
    val width = bimg.getWidth
    val height = bimg.getHeight
    (width, height)
  }

  def createImageFromHtml(outFile: Path, styleContent: String, bodyContent: String, w: Int, h: Int, resources: Seq[Path] = Seq(), workPath: Option[Path] = None): Unit = {
    def htmlTemplate(styleContent: String, bodyContent: String): String =
      s"""<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <meta name="viewport" content="width=device-width, initial-scale=0.7">
         |    <meta name="theme-color" content="#000">
         |    <title>Gaia Visual</title>
         |    <link rel="stylesheet" href="css/gaia.css">
         |    <style>
         |$styleContent
         |    </style>
         |</head>
         |<body>
         |$bodyContent
         |</body>
         |</html>
         |""".stripMargin


    Util.runWithTmpdir(workPath) { tmpWorkDir =>

      val resDir = tmpWorkDir.resolve("res")
      if Files.notExists(resDir) then Files.createDirectories(resDir)
      resources.foreach(r => Files.copy(r, resDir.resolve(r.getFileName)))

      val openingHtmlFile = tmpWorkDir.resolve(s"imgtxt.html")
      Util.writeString(openingHtmlFile, htmlTemplate(styleContent, bodyContent))

      Util.recursiveCopy(Path.of("src", "main", "html1", "css"), tmpWorkDir.resolve("css"))
      val chromiumRenderCmd = Seq("chromium", "--headless", "--no-sandbox", s"-window-size=$w,$h", s"--screenshot=${outFile.toAbsolutePath}", s"${openingHtmlFile.toAbsolutePath}")
      Util.runAllCommands(Seq(chromiumRenderCmd))
      println(s"Created image from html $outFile")
    }
  }

  case class StillImageGroup(
                              preview: Path,
                              full: Path,
                            )

  def stillImageGroups(gaiaImage: GaiaImage, workDir: Path): Seq[StillImageGroup] = {

    def allFiles(gaiaImage: GaiaImage): Seq[Path] = {
      val outDir = workDir.resolve("out")
      if Files.notExists(outDir) then Seq()
      else {
        val imgDir = outDir.resolve(gaiaImage.id)
        if Files.notExists(imgDir) then Seq()
        else {
          val stillDir = imgDir.resolve("stills")
          if Files.notExists(stillDir) then Seq()
          else {
            Files.list(stillDir).toScala(Seq)
          }
        }
      }
    }

    def stillImageGroup(triples: Seq[(String, String, String)]): Option[StillImageGroup] = {
      def filterImageType(imageType: String): Seq[Path] = {
        triples.filter((_, t, _) => t == imageType).map { case (a, b, c) => Path.of(s"$a-$b-$c") }
      }

      val prev = filterImageType("prev").headOption
      val full = filterImageType("full").headOption
      if prev.isDefined && full.isDefined then Some(StillImageGroup(prev.get, full.get))
      else None
    }

    val pattern = "(.*)-(full|prev)-(.*)".r
    allFiles(gaiaImage)
      .flatMap { path =>
        val fnam = path.toString
        fnam match {
          case pattern(a, b, c) => Some(a, b, c)
          case _ => None
        }
      }
      .groupBy { case (a, _, _) => a }
      .toSeq
      .map((_, v) => v)
      .flatMap(stillImageGroup)
  }

  def imageResources(imageId: String, extention: String, workPath: Path): Seq[Path] = {
    val mdir = Util.fileDirInOutDir(workPath, imageId).resolve("models")
    if Files.notExists(mdir) then throw IllegalStateException(s"Directory 'models' missing for $imageId")
    Files.list(mdir).toScala(Seq).filter(p => p.getFileName.toString.toLowerCase.endsWith(extention))
  }

  def valueToIndex(size: Double, count: Int)(value: Double): Option[Int] = {
    val valueRelative = value / size
    if value >= 0 then if valueRelative >= count then None else Some(valueRelative.toInt)
    else if valueRelative <= -count then None else Some(valueRelative.toInt - 1)
  }
}


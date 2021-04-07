package gaia

import java.io.{BufferedReader, IOException, InputStream, InputStreamReader, PrintWriter}
import java.nio.file.{Files, Path, StandardCopyOption}
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors, Future}
import java.util.{Locale, stream}
import scala.collection.JavaConverters._
import scala.util.Random

object Util {

  import X3d.{Color}
  import Vector._


  def ranOff(factor: Double): Double = (Random.nextDouble() - 0.5) * factor

  def datapath: Path = {
    val home = Path.of(System.getProperty("user.home"))
    val result = home.resolve(Path.of("gaia", "data"))
    if !Files.exists(result) then
      Files.createDirectories(result)
    result
  }

  def outpath: Path = {
    val result = datapath.resolve("out")
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
 
  def modelPath: Path = htmlPath.resolve("models")

  def htmlPath: Path = Path.of("src", "main", "html")

  private def xValues(cnt: Int): Seq[Double] = {
    require(cnt >= 2, s"cnt:$cnt must be greater equal 2")
    val step = 1.0 / (cnt - 1)
    (0 until cnt)
      .map(i => i * step)
  }

  private def linearFunction(startValue: Double, endValue: Double): (Double) => Double = {
    def lin(a: Double, k: Double)(x: Double): Double = a + k * x
    val k = endValue - startValue
    lin(startValue, k)(_)
  }

  private def squaredFunction(startValue: Double, endValue: Double): (Double) => Double = {
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
  
  def recursiveCopy(sourceDir: Path, destinationDir: Path) = {
    if (Files.notExists(destinationDir)) Files.createDirectories(destinationDir)
    Files.walk(sourceDir).forEach((sourcePath: Path) => {
      def foo(sourcePath: Path) = try {
        val targetPath = destinationDir.resolve(sourceDir.relativize(sourcePath))
        printf("Copying %s to %s%n", sourcePath, targetPath)
        Files.copy(sourcePath, targetPath, StandardCopyOption.REPLACE_EXISTING)
      } catch {
        case ex: IOException => printf("I/O error: %s%n", ex)
      }
      foo(sourcePath)
    })
  }

  def runAllCommands(cmds: Iterable[Iterable[String]]): Unit = {

    class StreamGobbler(val inputStream: InputStream,   val errorStream: InputStream) extends Runnable {
      private def handleInputStream(in: InputStream) = {
        val br = new BufferedReader(new InputStreamReader(in))
        try {
          br.lines().forEach(line => println(line))
        } finally {
          br.close()
        }
      }

      override def run(): Unit = {
        handleInputStream(inputStream)
        handleInputStream(errorStream)
      }

    }

    val gobbleExec: ExecutorService = Executors.newSingleThreadExecutor()
    val procExec: ExecutorService = Executors.newFixedThreadPool(Runtime.getRuntime.availableProcessors)
    try {
      def start(cmd: List[String]): Int = {
        val process = new ProcessBuilder()
          .command(cmd.asJava)
          .start()

        val streamGobbler = StreamGobbler(process.getInputStream(), process.getErrorStream)
        gobbleExec.submit(streamGobbler)
        println("--- started command: " + cmd.mkString(" "))
        val procResult = process.waitFor()
        println("--- finished command: " + cmd.mkString(" ") + " " + procResult)
        procResult
      }

      val futures = for (cmd <- cmds) yield {
        Thread.sleep(500)
        procExec.submit(() => start(cmd.toList))
      }
      var states = futures.map(f => f.isDone)
      while (!states.forall(s => s)) {
        Thread.sleep(500)
        val all = states.size
        val done = states.filter(v => v).size
        println(s"--- Check futures. $done of $all done")
        states =  futures.map(f => f.isDone)
      }
      val exits = futures.map(f => f.get())
      println(s"--- finished all commands. Exit values: ${exits.mkString(",")}")
      val errros = exits.filter(_ != 0)
      if !errros.isEmpty then throw IllegalStateException("At least one of the processes finiched with error")
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

}


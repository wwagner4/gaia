package gaia

import gaia.X3d.Color

import java.io.{IOException, PrintWriter}
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.JavaConverters._
import scala.language.implicitConversions
import scala.util.Random

object Util {

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

  def toVec(ra: Double, dec: Double, dist: Double): X3d.Vec = {
    val r = math.Pi / 180
    val x = math.cos(ra * r) * math.cos(dec * r) * dist
    val y = math.sin(ra * r) * math.cos(dec * r) * dist
    val z = math.sin(dec * r) * dist
    X3d.Vec(x, y, z)
  }

  def modelPath: Path = htmlPath.resolve("models")

  def htmlPath: Path = Path.of("src", "main", "html")

  def test(id: String): Unit = {
    println(s"running $id")
    val s = 0.5
    val e = 1
    val c = 5
    val r = squaredValues(s, e, c)
      .map(_.toString)
      .mkString(",")
    println(s"$s - $e : $c ($r)")
  }

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


}


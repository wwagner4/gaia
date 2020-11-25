package gaia

import java.nio.file.Files

case class FileNameMd5(
                   md5sum: String,
                   fname: String,
                   )

object Data {
  
  def readMeta(): Unit = {
    readFilnameMd5
      .foreach(println(_))
  }
  
  def readFilnameMd5: Iterator[FileNameMd5] = {
    val is = this.getClass.getClassLoader.getResourceAsStream("MD5SUM.txt")
    scala.io.Source.fromInputStream(is)
      .getLines()
      .take(10)
      .map(line => line.split("\\s+"))
      .map(a => FileNameMd5(a(0).trim, a(1).trim))
      .filter(_.fname.startsWith("GaiaSource"))
  }
}

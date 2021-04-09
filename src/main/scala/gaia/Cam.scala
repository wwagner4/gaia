package gaia

import gaia.Main.GaiaImage
import gaia.X3d.Shapable

import java.nio.file.{Files, Path}

object Cam {

  import Vector._
  import X3d._

  case class Camera(
                     name: String,
                     pos: Vec,
                     dir: Vec,
                   )

  case class VQuality(
                       steps: Int,
                       geometry: (Int, Int) = (600, 420),
                       frameRates: Seq[Int] = Seq(1, 2, 4),
                       antiAlias: Int = 4,
                     )

  enum VideoQuality(val quality: VQuality) {

    case VGA extends VideoQuality(VQuality(60, (600, 420), Seq(1, 2, 4), 3))

    case SVGA extends VideoQuality(VQuality(120, (800, 600), Seq(2, 4, 8), 3))

    case HD extends VideoQuality(VQuality(240, (1200, 720), Seq(4, 8, 16), 2))

    case FullHD extends VideoQuality(VQuality(600, (1920, 1080), Seq(10, 20, 40), 2))

    case UltraHD extends VideoQuality(VQuality(1620, (2560, 1440), Seq(27), 1))

    case _4k extends VideoQuality(VQuality(1620, (3840, 2160), Seq(27), 1))

    case _4kwide extends VideoQuality(VQuality(1620, (4098, 2160), Seq(27), 1))

  }


  def sund1(gaiaImage: GaiaImage, workPath: Path): Unit = {
    val quality = VideoQuality._4k
    sund1Base(gaiaImage, workPath, quality)
  }

  def sund1Prev(gaiaImage: GaiaImage, workPath: Path): Unit = {
    val quality = VideoQuality.VGA
    sund1Base(gaiaImage, workPath, quality)
  }

  private def sund1Base(gaiaImage: GaiaImage, workPath: Path, quality: VideoQuality) = {
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    mkVideo(gaiaImage.id, shapables, cameras(0, 20, 0.05), quality, gaiaImage.backColor, workPath)
  }

  def gc1(gaiaImage: GaiaImage, workPath: Path): Unit = {
    g1cBase(gaiaImage, workPath, VideoQuality._4k)
  }

  def gc1Prev(gaiaImage: GaiaImage, workPath: Path): Unit = {
    g1cBase(gaiaImage, workPath, VideoQuality.VGA)
  }

  private def g1cBase(gaiaImage: GaiaImage, workPath: Path, quality: VideoQuality) = {
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    mkVideo(gaiaImage.id, shapables, cameras(0, 20, 4), quality, gaiaImage.backColor, workPath)
  }

  def mkVideo(
               id: String,
               shapables: Seq[Shapable],
               fCams: Int => Seq[Camera],
               videoQuality: VideoQuality,
               bc: Color,
               workPath: Path) = {
    val outDir = Files.createTempDirectory(id)
    if Files.notExists(outDir) then Files.createDirectories(outDir)
    val videoOutDir = workPath.resolve(id).resolve("videos")
    if Files.notExists(videoOutDir) then Files.createDirectories(videoOutDir)
    val numlen = 4
    val x3d0File = outDir.resolve(s"${id}.x3d")
    val quality: VQuality = videoQuality.quality
    val cams: Seq[Camera] = fCams(quality.steps)
    val xml = X3d.createXml(shapables, x3d0File.getFileName.toString, bc, cams)
    gaia.Util.writeString(x3d0File, xml)
    println(s"wrote to $x3d0File")
    val commands = cams
      .zipWithIndex
      .map { (c, i) =>
        val iFmt = "%0" + numlen + "d"
        val iStr = iFmt.format(i)

        
        val x3dFile = outDir.resolve(s"${id}_${iStr}.x3d")
        Files.copy(x3d0File, x3dFile)
        println(s"copied to $x3dFile")

        val geometryStr = s"${quality.geometry._1}x${quality.geometry._2}"
        val model = x3dFile.toAbsolutePath.toString
        val image = outDir.resolve(s"${id}_${iStr}.png").toAbsolutePath.toString
        Seq("view3dscene", model, "--anti-alias", quality.antiAlias.toString, "--viewpoint", i.toString,
          "--geometry", geometryStr, "--screenshot", "0", image)
      }

    println(commands.map(c => c.mkString(" ")).mkString("\n"))
    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")
    val iFmtFf = "%0" + numlen + "d"
    val imgFile = outDir.resolve(s"${id}_$iFmtFf.png").toAbsolutePath.toString
    val videoId = videoQuality.toString
    val outFiles = quality.frameRates.map { fr =>
      videoOutDir.resolve(s"${id}_${videoId}_$fr.mp4").toAbsolutePath.toString
    }
    val cmd = quality.frameRates.zip(outFiles).map { case (fr, outFile) =>
      Seq("ffmpeg", "-y", "-r", fr.toString, "-i", imgFile, outFile)
    }
    Util.runAllCommands(cmd)
    val outFilesStr = outFiles.map("- " + _).mkString("\n")
    println("wrote video snipplets to\n" + outFilesStr)
  }


  def degSteps(n: Int): Seq[Double] = {
    val d = 360.0 / n

    def rs(x: Double, res: List[Double]): List[Double] = {
      if x >= 360 then res
      else rs(x + d, x :: res)
    }

    rs(0, List.empty[Double]).reverse
  }

  def cameras(ra: Int, dec: Int, radius: Double, name: String = "gaiadefined")(steps: Int): Seq[Camera] = {
    degSteps(steps)
      .zip(LazyList.continually(PolarVec(radius, 0, 0)))
      .map { (d, v) => v.copy(ra = degToRad(d)) }
      .map(pv => pv.toVec)
      .map(v => v.roty(-degToRad(dec)))
      .map(v => v.rotz(degToRad(ra)))
      .zipWithIndex
      .map { (v, i) =>
        val nam = f"${name}_${i}%04d"
        val dir = Vec.zero.sub(v)
        Camera(nam, v, dir)
      }
  }


}

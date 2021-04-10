package gaia

import gaia.Main.GaiaImage
import gaia.X3d.Shapable

import java.nio.file.{Files, Path}
import scala.collection.SeqView.Reverse
import scala.util.Random

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


  def sund1Still(gaiaImage: GaiaImage, workPath: Path): Unit = {
    val quality = VideoQuality.UltraHD
    val camsWithIndex = Seq(
      cameras(0, 20, 0.05)(quality.quality.steps).zipWithIndex,
      cameras(0, -45, 0.1)(quality.quality.steps).zipWithIndex,
    ).flatten
    val shapables: Seq[Shapable] = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)

    Random.setSeed(92838472983L)
    val commands = Random.shuffle(camsWithIndex)
      .take(20)
      .zipWithIndex
      .map { case ((cam, vpId), stillId) => mkStillCommand(gaiaImage.id, stillId.toString, vpId, cam, quality, shapables, workPath) }

    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")

  }

  def sund1Video(quality: VideoQuality)(gaiaImage: GaiaImage, workPath: Path) = {
    case class CameraConfig(
                             id: String,
                             cams: Seq[Camera],
                           )

    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val cams = Seq(
      CameraConfig("near", cameras(0, 20, 0.05)(quality.quality.steps)),
      CameraConfig("far", cameras(0, -45, 0.1, reverse = true)(quality.quality.steps)),
    )

    cams.foreach { cconf =>
      mkVideo(gaiaImage.id, cconf.id, shapables, cconf.cams, quality, gaiaImage.backColor, workPath)
    }
  }

  def g1cVideo(quality: VideoQuality)(gaiaImage: GaiaImage, workPath: Path) = {
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val cams = cameras(0, 20, 4)(quality.quality.steps)
    mkVideo(gaiaImage.id, "00", shapables, cams, quality, gaiaImage.backColor, workPath)
  }

  def mkStillCommand(
                      imageId: String,
                      stillId: String,
                      vpId: Int,
                      cam: Camera,
                      vquality: VideoQuality,
                      shapables: Seq[Shapable],
                      workPath: Path): Iterable[String] = {
    val stillOutDir = workPath.resolve(imageId).resolve("stills")
    if Files.notExists(stillOutDir) then Files.createDirectories(stillOutDir)
    val iStr = stillId

    val x3dFile = stillOutDir.resolve(s"${imageId}_${iStr}.x3d")

    val quality = vquality.quality
    val geometryStr = s"${quality.geometry._1}x${quality.geometry._2}"
    val model = x3dFile.toAbsolutePath.toString
    val image = stillOutDir.resolve(s"${imageId}_${iStr}.png").toAbsolutePath.toString
    Seq("view3dscene", model, "--anti-alias", quality.antiAlias.toString, "--viewpoint", vpId.toString,
      "--geometry", geometryStr, "--screenshot", "0", image)
  }


  def mkVideo(
               imageId: String,
               videoId: String,
               shapables: Seq[Shapable],
               cams: Seq[Camera],
               videoQuality: VideoQuality,
               bc: Color,
               workPath: Path) = {
    val outDir = Files.createTempDirectory(imageId)
    if Files.notExists(outDir) then Files.createDirectories(outDir)
    val videoOutDir = workPath.resolve(imageId).resolve("videos")
    if Files.notExists(videoOutDir) then Files.createDirectories(videoOutDir)
    val numlen = 4
    val x3d0File = outDir.resolve(s"${imageId}.x3d")
    val quality: VQuality = videoQuality.quality
    val xml = X3d.createXml(shapables, x3d0File.getFileName.toString, bc, cams)
    gaia.Util.writeString(x3d0File, xml)
    println(s"wrote to $x3d0File")
    val commands = cams
      .zipWithIndex
      .map { (c, i) =>
        val iFmt = "%0" + numlen + "d"
        val iStr = iFmt.format(i)

        val x3dFile = outDir.resolve(s"${imageId}_${iStr}.x3d")
        Files.copy(x3d0File, x3dFile)
        println(s"copied to $x3dFile")

        val geometryStr = s"${quality.geometry._1}x${quality.geometry._2}"
        val model = x3dFile.toAbsolutePath.toString
        val image = outDir.resolve(s"${imageId}_${iStr}.png").toAbsolutePath.toString
        Seq("view3dscene", model, "--anti-alias", quality.antiAlias.toString, "--viewpoint", i.toString,
          "--geometry", geometryStr, "--screenshot", "0", image)
      }

    println(commands.map(c => c.mkString(" ")).mkString("\n"))
    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")
    val iFmtFf = "%0" + numlen + "d"
    val imgFile = outDir.resolve(s"${imageId}_$iFmtFf.png").toAbsolutePath.toString
    val qualStr = videoQuality.toString
    val outFiles = quality.frameRates.map { fr =>
      videoOutDir.resolve(s"${imageId}_${videoId}_${qualStr}_$fr.mp4").toAbsolutePath.toString
    }
    val cmd = quality.frameRates.zip(outFiles).map { case (fr, outFile) =>
      Seq("ffmpeg", "-y", "-r", fr.toString, "-i", imgFile, outFile)
    }
    Util.runAllCommands(cmd)
    val outFilesStr = outFiles.map("- " + _).mkString("\n")
    println("wrote video snipplets to\n" + outFilesStr)
  }


  def degSteps(n: Int, reverse: Boolean): Seq[Double] = {
    val d = 360.0 / n
    if reverse then {
      def rs(x: Double, res: List[Double]): List[Double] = {
        if x <= 0 then res
        else rs(x - d, x :: res)
      }

      rs(360, List.empty[Double]).reverse
    }
    else {
      def rs(x: Double, res: List[Double]): List[Double] = {
        if x >= 360 then res
        else rs(x + d, x :: res)
      }

      rs(0, List.empty[Double]).reverse
    }
  }

  def cameras(ra: Int, dec: Int, radius: Double, name: String = "gaiadefined", reverse: Boolean = false)(steps: Int): Seq[Camera] = {
    degSteps(steps, reverse)
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

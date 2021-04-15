package gaia

import java.nio.file.{Files, Path}
import scala.collection.SeqView.Reverse
import scala.util.Random

object Cam {

  import Vector._
  import X3d._
  import Main._

  case class Camera(
                     name: String,
                     pos: Vec,
                     dir: Vec,
                   )

  def mapVideospeed(videoSpeed: VideoSpeed): Int = videoSpeed match {
    case VideoSpeed.slow => 5000
    case VideoSpeed.medium => 2500
    case VideoSpeed.fast => 2000
  }

  def sund1Still(gaiaImage: GaiaImage, workPath: Path, preview: Boolean = false): Unit = {
    val vq = VideoQuality.UltraHD
    val quality = gaiaImage.videoQuality
    val steps = if preview then 100 else mapVideospeed(gaiaImage.videoSpeed)
    val camsWithIndex = Seq(
      cameras(0, 20, 0.05)(steps).zipWithIndex,
      cameras(0, -45, 0.1)(steps).zipWithIndex,
    ).flatten
    val shapables: Seq[Shapable] = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)

    Random.setSeed(92838472983L)
    val commands = Random.shuffle(camsWithIndex)
      .take(20)
      .zipWithIndex
      .map { case ((cam, vpId), stillId) =>
        mkStillCommand(gaiaImage.id, stillId.toString, cam, quality, 2, shapables, gaiaImage.backColor, workPath)
      }

    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")

  }

  def sund1Video(gaiaImage: GaiaImage, workPath: Path, preview: Boolean = false) = {
    case class CameraConfig(
                             id: String,
                             cams: Seq[Camera],
                           )
    val vq = if preview then VideoQuality.VGA else gaiaImage.videoQuality
    val quality = gaiaImage.videoQuality
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val steps = if preview then 100 else mapVideospeed(gaiaImage.videoSpeed)
    val cams = Seq(
      CameraConfig("near", cameras(0, 20, 0.05)(steps)),
      CameraConfig("far", cameras(0, -45, 0.1, reverse = true)(steps)),
    )

    cams.foreach { cconf =>
      mkVideo(gaiaImage.id, cconf.id, shapables, cconf.cams, quality, 2, Seq(10), gaiaImage.videoSpeed.toString, gaiaImage.backColor, workPath)
    }
  }

  def g1cVideo(gaiaImage: GaiaImage, workPath: Path, preview: Boolean = false) = {
    val quality = if preview then VideoQuality.VGA else gaiaImage.videoQuality
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val steps = if preview then 100 else mapVideospeed(gaiaImage.videoSpeed)
    val cams = cameras(0, 20, 4)(steps)
    mkVideo(gaiaImage.id, "00", shapables, cams, quality, 3, Seq(10), gaiaImage.videoSpeed.toString, gaiaImage.backColor, workPath)
  }

  def mkStillCommand(
                      imageId: String,
                      stillId: String,
                      cam: Camera,
                      qual: VideoQuality,
                      antiAlias: Int,
                      shapables: Seq[Shapable],
                      backColor: Color,
                      workPath: Path): Iterable[String] = {
    val stillOutDir = workPath.resolve(imageId).resolve("stills")
    if Files.notExists(stillOutDir) then Files.createDirectories(stillOutDir)
    val tmpWorkDir = Files.createTempDirectory(imageId)
    val x3dFile = tmpWorkDir.resolve(s"${imageId}_${stillId}.x3d")
    val xml = X3d.createXml(shapables, x3dFile.getFileName.toString, backColor, Seq(cam))
    gaia.Util.writeString(x3dFile, xml)

    val geometryStr = s"${qual.width}x${qual.height}"
    val x3dPath = x3dFile.toAbsolutePath.toString
    val stillFile = stillOutDir.resolve(s"${imageId}_${stillId}.png").toAbsolutePath.toString
    Seq("view3dscene", x3dPath, "--anti-alias", antiAlias.toString, "--viewpoint", "0",
      "--geometry", geometryStr, "--screenshot", "0", stillFile)
  }


  def mkVideo(
               imageId: String,
               videoId: String,
               shapables: Seq[Shapable],
               cams: Seq[Camera],
               quality: VideoQuality,
               antiAlias: Int,
               frameRates: Seq[Int],
               speed: String,
               backColor: Color,
               workPath: Path) = {
    val numlen = 4

    val tmpWorkDir = Files.createTempDirectory(imageId)
    val videoOutDir = workPath.resolve(imageId).resolve("videos")
    if Files.notExists(videoOutDir) then Files.createDirectories(videoOutDir)
    val x3d0File = tmpWorkDir.resolve(s"${imageId}.x3d")
    val xml = X3d.createXml(shapables, x3d0File.getFileName.toString, backColor, cams)
    gaia.Util.writeString(x3d0File, xml)
    println(s"wrote to $x3d0File")
    val commands = cams
      .zipWithIndex
      .map { (c, i) =>
        val iFmt = "%0" + numlen + "d"
        val iStr = iFmt.format(i)

        val x3dFile = tmpWorkDir.resolve(s"${imageId}_${iStr}.x3d")
        Files.copy(x3d0File, x3dFile)
        println(s"copied to $x3dFile")

        val geometryStr = s"${quality.width}x${quality.height}"
        val model = x3dFile.toAbsolutePath.toString
        val image = tmpWorkDir.resolve(s"${imageId}_${iStr}.png").toAbsolutePath.toString
        Seq("view3dscene", model, "--anti-alias", antiAlias.toString, "--viewpoint", i.toString,
          "--geometry", geometryStr, "--screenshot", "0", image)
      }

    println(commands.map(c => c.mkString(" ")).mkString("\n"))
    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")
    val iFmtFf = "%0" + numlen + "d"
    val imgFile = tmpWorkDir.resolve(s"${imageId}_$iFmtFf.png").toAbsolutePath.toString
    val qualStr = quality.toString
    val outFiles = frameRates.map { fr =>
      videoOutDir.resolve(s"${imageId}_${videoId}_${qualStr}_${speed}_$fr.mp4").toAbsolutePath.toString
    }
    val cmd = frameRates.zip(outFiles).map { case (fr, outFile) =>
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

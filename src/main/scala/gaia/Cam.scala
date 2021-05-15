package gaia

import com.sun.tools.javac.code.Preview

import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.SeqView.Reverse
import scala.util.Random

object Cam {

  import Vector._
  import X3d._
  import Gaia._

  val numLen = 4
  val stillImgCnt = 4

  def mkVideoCameraConfig(gaiaImage: GaiaImage, cameraConfigs: Seq[CameraConfig], workPath: Path, videoQuality: VideoQuality): Unit = {
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val frameRate = videoQuality.frameRate

    val cmds = cameraConfigs.map { cfg =>
      val duration = cfg.durationInSec
      val steps = duration * frameRate
      val cams = cfg.cams(steps)
      mkVideoCmds(gaiaImage.id, cfg.id, shapables, cams, cfg.modelRotation, videoQuality, duration, gaiaImage.backColor, workPath)
    }
    val allView3dCmds = cmds.map((view3dCmd, _) => view3dCmd)
    Util.runAllCommands(allView3dCmds)
    val allFfmpegCmds = cmds.map((_, ffmpegCmd) => ffmpegCmd)
    allFfmpegCmds.foreach(cmd => Util.runAllCommands(Seq(cmd)))
  }

  def mkStillcameraConfig(gaiaImage: GaiaImage, cameraConfigs: Seq[CameraConfig], workPath: Path): Unit = {

    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val frameRate = gaiaImage.videoQuality.frameRate

    val cmds = cameraConfigs.map { cfg =>
      val duration = cfg.durationInSec
      val steps = duration * frameRate
      val cams = cfg.cams(steps)
      val imageCount = gaiaImage.videoQuality.frameRate * duration
      Random.shuffle(0 until imageCount).take(stillImgCnt).zipWithIndex.map((time, i) =>
        mkStillCommand(gaiaImage.id, cfg.id, i.toString, shapables, cams, cfg.modelRotation, gaiaImage.videoQuality, duration, time, gaiaImage.backColor, workPath))
    }
    cmds.foreach(cs => cs.foreach(c => Util.runAllCommands(Seq(c))))
  }

  def mkStillCommand(
                      imageId: String,
                      videoId: String,
                      stillId: String,
                      shapables: Seq[Shapable],
                      cams: Seq[Camera],
                      modelRotation: Rotation1,
                      quality: VideoQuality,
                      cycleIntervalInSeconds: Int,
                      time: Int,
                      backColor: Color,
                      workPath: Path): Seq[String] = {
    val stillOutDir = workPath.resolve(imageId).resolve("stills")
    if Files.notExists(stillOutDir) then Files.createDirectories(stillOutDir)
    val tmpWorkDir = Files.createTempDirectory(imageId)


    val x3dFile = tmpWorkDir.resolve(s"$imageId-$videoId-$stillId.x3d")
    val xml = X3d.createCamAnimatedXml(shapables, cams, backColor, cycleIntervalInSeconds, modelRotation)
    gaia.Util.writeString(x3dFile, xml)
    println(s"wrote to $x3dFile")
    val geometryStr = s"${quality.videoResolution.width}x${quality.videoResolution.height}"
    val timeStep = 1.0 / quality.frameRate
    val imageFiles = stillOutDir.resolve(s"$imageId-$videoId-$stillId-@counter($numLen).png")
    val antiAliasing = quality.antiAliasing

    Seq("view3dscene", s"${x3dFile.toAbsolutePath}",
      "--anti-alias", s"$antiAliasing", "--geometry", s"$geometryStr",
      "--screenshot-range", time.toString, f"$timeStep%.4f", "1", s"$imageFiles")

  }

  def mkVideoCmds(
                   imageId: String,
                   videoId: String,
                   shapables: Seq[Shapable],
                   cams: Seq[Camera],
                   modelRotation: Rotation1,
                   quality: VideoQuality,
                   cycleIntervalInSeconds: Int,
                   backColor: Color,
                   workPath: Path): (Seq[String], Seq[String]) = {
    val tmpWorkDir = Files.createTempDirectory(imageId)
    val videoOutDir = workPath.resolve(imageId).resolve("videos")
    if Files.notExists(videoOutDir) then Files.createDirectories(videoOutDir)
    val x3d0File = tmpWorkDir.resolve(s"$imageId.x3d")
    val xml = X3d.createCamAnimatedXml(shapables, cams, backColor, cycleIntervalInSeconds, modelRotation)
    gaia.Util.writeString(x3d0File, xml)
    println(s"wrote to $x3d0File")
    val geometryStr = s"${quality.videoResolution.width}x${quality.videoResolution.height}"
    val imageCount = quality.frameRate * cycleIntervalInSeconds
    val timeStep = 1.0 / quality.frameRate
    val imageFiles = tmpWorkDir.resolve(s"img@counter($numLen).png")
    val antiAliasing = quality.antiAliasing
    val frameRate = quality.frameRate
    val iFmtFf = "%0" + numLen + "d"
    val imgFile = tmpWorkDir.resolve(s"img$iFmtFf.png").toAbsolutePath.toString
    val videoFile = videoOutDir.resolve(s"${imageId}_${videoId}_$frameRate.mp4").toAbsolutePath.toString

    val view3dCmd = Seq("view3dscene", s"${x3d0File.toAbsolutePath}",
      "--anti-alias", s"$antiAliasing", "--geometry", s"$geometryStr",
      "--screenshot-range", "0", f"$timeStep%.4f", s"$imageCount", s"$imageFiles")
    val ffmpegCmd = Seq("ffmpeg", "-y", "-r", frameRate.toString, "-i", imgFile, "-c:v", "libx265", videoFile)
    (view3dCmd, ffmpegCmd)
  }


  def degSteps(n: Int, reverse: Boolean): Seq[Double] = {
    val d = 360.0 / n
    if reverse then {
      @tailrec
      def rs(x: Double, res: List[Double]): List[Double] = {
        if x <= 0 then res
        else rs(x - d, x :: res)
      }

      rs(360, List.empty[Double]).reverse
    }
    else {
      @tailrec
      def rs(x: Double, res: List[Double]): List[Double] = {
        if x >= 360 then res
        else rs(x + d, x :: res)
      }

      rs(0, List.empty[Double]).reverse
    }
  }

  def cameras(raInDeg: Int, decInDeg: Int, radius: Double,
              name: String = "gaiadefined", reverse: Boolean = false,
              eccentricity: Double = 0.0, offset: Vec = Vec(0, 0, 0), center: Vec = Vec(0, 0, 0))
             (frames: Int): Seq[Camera] = {
    degSteps(frames, reverse)
      .map(degToRad)
      .map(rad => ellipse(radius, eccentricity, rad))
      .map(v => v.roty(-degToRad(decInDeg)))
      .map(v => v.rotz(degToRad(raInDeg)))
      .map(v => v.add(offset))
      .zipWithIndex
      .map { (v: Vec, i: Int) =>
        val nam = f"${name}_$i%04d"
        val dir = center.sub(v)
        Camera(nam, v, dir, i)
      }
  }

  def ellipse(r: Double, e: Double, degInRad: Double): Vec = {
    val v1 = PolarVec(r, degInRad, 0).toVec
    val r2 = r * r
    val x2 = v1.x * v1.x
    val e2 = e * e
    val y =
      if v1.y > 0
      then math.sqrt(r2 * (1.0 - e2) * (1.0 - x2 / r2))
      else -math.sqrt(r2 * (1.0 - e2) * (1.0 - x2 / r2))
    Vec(v1.x, y, v1.z)
  }

}

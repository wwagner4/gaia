package gaia

import com.sun.tools.javac.code.Preview

import java.nio.file.{Files, Path}
import scala.annotation.tailrec
import scala.collection.SeqView.Reverse
import scala.util.Random
import scala.jdk.StreamConverters._

object Cam {

  import Vector._
  import X3d._
  import Gaia._
  import Hp1._

  val numLen = 4
  val stillImgCnt = 4

  def mkVideoCameraConfig(gaiaImage: GaiaImage, cameraConfigs: Seq[CameraConfig], workPath: Path, videoQuality: VideoQuality): Unit = {
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val frameRate = videoQuality.frameRate

    Util.runWithTmpdir() { tmpDir =>
      val cmds = cameraConfigs
        .map { cfg =>
          val duration = cfg.durationInSec
          val steps = duration * frameRate
          val cams = cfg.cams(steps)
          val td = tmpDir.resolve(cfg.id)
          if Files.notExists(td) then Files.createDirectories(td)
          mkVideoCmds(gaiaImage.id, cfg.id, shapables, cams, cfg.modelRotation, videoQuality, duration, gaiaImage.backColor, workPath, td)
        }
      val allView3dCmds = cmds.map((view3dCmd, _) => view3dCmd)
      Util.runAllCommands(allView3dCmds)
      val allFfmpegCmds = cmds.map((_, ffmpegCmd) => ffmpegCmd)
      allFfmpegCmds.foreach(cmd => Util.runAllCommands(Seq(cmd)))
    }
  }

  def mkStillcameraConfig(gaiaImage: GaiaImage, cameraConfigs: Seq[CameraConfig], workPath: Path): Unit = {

    Util.runWithTmpdir() { tmpDir =>
      val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
      val frameRate = gaiaImage.videoQuality.frameRate

      val cmds = cameraConfigs.map { cfg =>
        val duration = cfg.durationInSec
        val steps = duration * frameRate
        val cams = cfg.cams(steps)
        val imageCount = gaiaImage.videoQuality.frameRate * duration
        Random.shuffle(0 until imageCount).take(stillImgCnt).zipWithIndex.map { (time, i) =>
          val iid = i.toString
          val iidFull = s"$iid-full"
          val iidPrev = s"$iid-prev"
          Seq(
            mkStillCommand(gaiaImage.id, cfg.id, iidFull, shapables, cams, cfg.modelRotation, gaiaImage.videoQuality, duration, time, gaiaImage.backColor, workPath, tmpDir),
            mkStillCommand(gaiaImage.id, cfg.id, iidPrev, shapables, cams, cfg.modelRotation, VideoQuality.stillPreview, duration, time, gaiaImage.backColor, workPath, tmpDir),
          )
        }
      }
      cmds.foreach(cs => cs.foreach(c => Util.runAllCommands(c)))
    }
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
                      workPath: Path,
                      tmpWorkDir: Path): Seq[String] = {
    val stillOutDir = outPath(workPath).resolve(imageId).resolve("stills")
    if Files.notExists(stillOutDir) then Files.createDirectories(stillOutDir)

    val x3dFile = tmpWorkDir.resolve(s"$imageId-$videoId-$stillId.x3d")
    val xml = X3d.createCamAnimatedXml(shapables, cams, backColor, cycleIntervalInSeconds, modelRotation)
    gaia.Util.writeString(x3dFile, xml)
    println(s"wrote to $x3dFile")
    val geometryStr = s"${quality.videoResolution.width}x${quality.videoResolution.height}"
    val timeStep = 1.0 / quality.frameRate
    val imageFiles = stillOutDir.resolve(s"$imageId-$videoId-$stillId-@counter($numLen).png")
    val antiAliasing = quality.antiAliasing

    val view3dCmd = Seq("view3dscene", s"${x3dFile.toAbsolutePath}",
      "--anti-alias", s"$antiAliasing", "--geometry", s"$geometryStr",
      "--screenshot-range", time.toString, f"$timeStep%.4f", "1", s"$imageFiles")
    if Util.inDocker then xvfbCmd ++ view3dCmd
    else view3dCmd
  }

  def xvfbCmd = Seq("xvfb-run", "--auto-servernum", "-e", "/dev/stdout")

  def mkVideoCmds(
                   imageId: String,
                   videoId: String,
                   shapables: Seq[Shapable],
                   cams: Seq[Camera],
                   modelRotation: Rotation1,
                   quality: VideoQuality,
                   cycleIntervalInSeconds: Int,
                   backColor: Color,
                   workPath: Path,
                   tmpWorkDir: Path): (Seq[String], Seq[String]) = {
    val videoOutDir = outPath(workPath).resolve(imageId).resolve("videos")
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

    val view3dCmdPlain = Seq("view3dscene", s"${x3d0File.toAbsolutePath}",
      "--anti-alias", s"$antiAliasing", "--geometry", s"$geometryStr",
      "--screenshot-range", "0", f"$timeStep%.4f", s"$imageCount", s"$imageFiles")
    val view3dCmd = if Util.inDocker then xvfbCmd ++ view3dCmdPlain else view3dCmdPlain
    val ffmpegCmd = Seq("ffmpeg", "-y", "-r", frameRate.toString, "-i", imgFile, videoFile)
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

  def mkCompleteVideo(gi: GaiaImage, outFile: Path, workPath: Path): Unit = {
    val lenInMin = 1.5
    val lenSliceSec = 15.0
    val lenOpenSec = 15.0
    val lenCloseSec = 10.0
    val fadeDurationInSeconds = 3.0
    val fadeOverlappingInSeconds = 2.0 // from HP1:267

    val lenInSec = lenInMin * 60
    val sliceLenSec = lenInSec - lenOpenSec - lenCloseSec
    val sliceCnt = math.floor(sliceLenSec / lenSliceSec).toInt 

    Util.runWithTmpdir(Some(workPath)) { td =>
      val imgDir = Util.fileDirInOutDir(workPath, gi.id)
      val vidDir = imgDir.resolve("videos")
      if Files.notExists(vidDir) then throw IllegalThreadStateException("video directory $vidDir does not exist")
      val sizeGroups = Files.list(vidDir).toScala(Seq)
        .filter(p => p.getFileName.toString.endsWith("mp4"))
        .map { p =>
          val info = Hp1.vidInfo(p)
          ((info.width, info.height), info, p)
        }
        .groupBy(_._1)
      sizeGroups.toList.foreach { case ((w, h), videos) =>
        val vidInfo = videos.head._2
        val frameRate = vidInfo.frameRate.getNumerator
        val videosCnt = videos.size
        val sliceDurRelative = (lenSliceSec + 2 * (fadeDurationInSeconds + fadeOverlappingInSeconds)) / vidInfo.durationSeconds
        val slicePerVidCnt = math.ceil(sliceCnt.toDouble / videosCnt).toInt  

        val openingImg = Cred.createOpeningCredit(gi, w, h, td)
        val closingImg = Cred.createClosingCredit(gi, w, h, td)
        val openingCredPath = td.resolve(s"opening-cred-${gi.id}.mp4")
        val closingCredPath = td.resolve(s"closing-cred-${gi.id}.mp4")
        val scale = s"scale=$w:$h"
        val cmdOpening = Seq("ffmpeg", "-loop", "1", "-i", openingImg.toString, "-r", frameRate.toString, "-t", lenOpenSec.toString, "-vf",
          scale, openingCredPath.toString)
        val cmdClosing = Seq("ffmpeg", "-loop", "1", "-i", closingImg.toString, "-r", frameRate.toString, "-t", lenCloseSec.toString, "-vf",
          scale, closingCredPath.toString)
        Util.runAllCommands(Seq(cmdOpening, cmdClosing))

        val slicedVideos = videos.flatMap { case (_, _, p) =>
          val baseName = p.getFileName.toString.replaceFirst("[.][^.]+$", "")
          for i <- 1 to slicePerVidCnt yield vidSlice(p, s"$baseName-$i", td, durationRelative = sliceDurRelative)
        }
        val allVideos = Seq(openingCredPath) ++ Random.shuffle(slicedVideos).take(sliceCnt) ++ Seq(closingCredPath)
        val concatFile = Hp1.vidConcat(allVideos.toList, 0, td, fadeDurationInSeconds = fadeDurationInSeconds.toInt)
        println(s"Concatenated sliced files to $concatFile")
        Files.copy(concatFile, outFile)
        println(s"Copied result to $outFile")
      }
    }
  }

}

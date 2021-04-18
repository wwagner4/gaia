package gaia

import com.sun.tools.javac.code.Preview

import java.nio.file.{Files, Path}
import scala.annotation.tailrec
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

  type FCams = Int => Seq[Camera]

  case class CameraConfig(
                           id: String,
                           cams: FCams,
                           durationInSec: Int,
                         )

  val sund1cams = Seq(
    CameraConfig("near", cameras(0, 20, 0.05), 60),
    CameraConfig("far", cameras(0, -45, 0.1, reverse = true), 60),
  )

  def sund1Still(gaiaImage: GaiaImage, workPath: Path, preview: Boolean = false): Unit = {
    Random.setSeed(92838472983L)
    mkStill(gaiaImage, workPath)

  }

  def sund1Video(gaiaImage: GaiaImage, workPath: Path, preview: Boolean = false): Unit = {
    val quality = calcQuality(preview, gaiaImage)
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val frameRate = calcFrameRate(preview)
    sund1cams.foreach { cconf =>
      val steps = frameRate * cconf.durationInSec
      mkVideo(gaiaImage.id, cconf.id, shapables, cconf.cams(steps), quality, 2, frameRate, gaiaImage.backColor, workPath)
    }
  }

  val gc1cams = Seq(
    CameraConfig("near", cameras(0, 20, 4), 60),
    CameraConfig("far", cameras(0, -45, 6, reverse = true), 60),
  )

  def g1cVideo(gaiaImage: GaiaImage, workPath: Path, preview: Boolean = false): Unit = {
    val quality = calcQuality(preview, gaiaImage)
    val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
    val frameRate = calcFrameRate(preview)

    gc1cams.foreach{cfg =>
      val duration = cfg.durationInSec
      val steps = duration * frameRate
      val cams = cfg.cams(steps)
      mkVideo(gaiaImage.id, cfg.id, shapables, cams, quality, 3, frameRate, gaiaImage.backColor, workPath)
    }
    
  }

  def mkStill(gaiaImage: GaiaImage, workPath: Path): Unit = {
    val quality = gaiaImage.videoQuality
    val frameRate = calcFrameRate(false)
    val cams = sund1cams
      .flatMap { ccfg =>
        val steps = frameRate * ccfg.durationInSec
        Random.shuffle(ccfg.cams(steps)).take(10)
      }

    val shapables: Seq[Shapable] = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)

    val commands = cams
      .zipWithIndex
      .map { case (cam, stillId) =>
        mkStillCommand(gaiaImage.id, stillId.toString, cam, quality, 2, shapables, gaiaImage.backColor, workPath)
      }

    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")
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
    val x3dFile = tmpWorkDir.resolve(s"${imageId}_$stillId.x3d")
    val xml = X3d.createXml(shapables, x3dFile.getFileName.toString, backColor, Seq(cam))
    gaia.Util.writeString(x3dFile, xml)

    val geometryStr = s"${qual.width}x${qual.height}"
    val x3dPath = x3dFile.toAbsolutePath.toString
    val stillFile = stillOutDir.resolve(s"${imageId}_$stillId.png").toAbsolutePath.toString
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
                frameRate: Int,
                backColor: Color,
                workPath: Path): Unit = {
    val numlen = 4

    val tmpWorkDir = Files.createTempDirectory(imageId)
    val videoOutDir = workPath.resolve(imageId).resolve("videos")
    if Files.notExists(videoOutDir) then Files.createDirectories(videoOutDir)
    val x3d0File = tmpWorkDir.resolve(s"$imageId.x3d")
    val xml = X3d.createXml(shapables, x3d0File.getFileName.toString, backColor, cams)
    gaia.Util.writeString(x3d0File, xml)
    println(s"wrote to $x3d0File")
    val commands = cams.indices
      .map { i =>
        val iFmt = "%0" + numlen + "d"
        val iStr = iFmt.format(i)

        val x3dFile = tmpWorkDir.resolve(s"${imageId}_$iStr.x3d")
        Files.copy(x3d0File, x3dFile)
        println(s"copied to $x3dFile")

        val geometryStr = s"${quality.width}x${quality.height}"
        val model = x3dFile.toAbsolutePath.toString
        val image = tmpWorkDir.resolve(s"${imageId}_$iStr.png").toAbsolutePath.toString
        Seq("view3dscene", model, "--anti-alias", antiAlias.toString, "--viewpoint", i.toString,
          "--geometry", geometryStr, "--screenshot", "0", image)
      }

    println(commands.map(c => c.mkString(" ")).mkString("\n"))
    Util.runAllCommands(commands)
    println(s"finished ${commands.size} commands")
    val iFmtFf = "%0" + numlen + "d"
    val imgFile = tmpWorkDir.resolve(s"${imageId}_$iFmtFf.png").toAbsolutePath.toString
    val qualStr = quality.toString
    val outFile = videoOutDir.resolve(s"${imageId}_${videoId}_${qualStr}_$frameRate.mp4").toAbsolutePath.toString
    val cmd = Seq(Seq("ffmpeg", "-y", "-r", frameRate.toString, "-i", imgFile, outFile))
    Util.runAllCommands(cmd)
    println(s"wrote video snipplets to $outFile")
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

  def cameras(ra: Int, dec: Int, radius: Double,
              name: String = "gaiadefined", reverse: Boolean = false,
              eccentricity: Double = 0.0)(frames: Int): Seq[Camera] = {
    degSteps(frames, reverse)
      .map(degToRad)
      .map(rad => ellipse(radius, eccentricity, rad))
      .map(v => v.roty(-degToRad(dec)))
      .map(v => v.rotz(degToRad(ra)))
      .zipWithIndex
      .map { (v: Vec, i: Int) =>
        val nam = f"${name}_$i%04d"
        val dir = Vec.zero.sub(v)
        Camera(nam, v, dir)
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

  def calcFrameRate(preview: Boolean): Int = if preview then 2 else 27

  def calcQuality(preview: Boolean, gaiaImage: GaiaImage): VideoQuality =
    if preview then Main.VideoQuality.HD else gaiaImage.videoQuality

}

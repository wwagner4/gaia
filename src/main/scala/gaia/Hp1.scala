package gaia

import com.sun.tools.javac.api.JavacTaskPool.Worker
import org.apache.commons.lang3.math.Fraction
import java.nio.file.{Files, Path}

import scala.annotation.tailrec
import scala.jdk.StreamConverters._
import scala.util.Random


object Hp1 {

  import Gaia._
  import Util._

  enum CarouselEntryType:
    case IMAGE

  sealed trait FinalResource

  case class FRImage(imageFile: Path) extends FinalResource

  case class FRX3d(modelFile: Path) extends FinalResource

  case class FRAnimatedX3d(modelFile: Path) extends FinalResource

  case class FRVideo(videoUrl: String) extends FinalResource

  case object FRNull extends FinalResource

  case class CarouselEntry(
                            hpDirectory: Path,
                            previewImage: Path,
                            finalResource: FinalResource,
                            entryType: CarouselEntryType = CarouselEntryType.IMAGE,
                          ) {

    def toJson: String = {
      val fnam = previewImage.getFileName.toString
      val imgStr = s"images/$fnam"
      val finalRes = finalResource match {
        case FRImage(imageFile) => s"'images/${imageFile.getFileName}'"
        case FRX3d(modelFile) => s"'models/${modelFile.getFileName}'"
        case FRAnimatedX3d(modelFile) => s"'models/${modelFile.getFileName}'"
        case FRVideo(videoUrl) => s"'$videoUrl'"
        case FRNull => "null"
      }
      s"""{
         |  entry_type: '${entryType.toString}',
         |  image_name: '$imgStr',
         |  image_name_full: $finalRes
         |}
         |""".stripMargin
    }

    def copyResources(): Unit = {
      val imagesDir = Util.fileDirFromDir(hpDirectory, "images")
      val modelsDir = Util.fileDirFromDir(hpDirectory, "models")
      val animatedModelsDir = Util.fileDirFromDir(hpDirectory, "models")
      Util.fileCopy(previewImage, imagesDir)
      finalResource match {
        case r: FRImage => Util.fileCopy(r.imageFile, imagesDir)
        case r: FRX3d => Util.fileCopy(r.modelFile, modelsDir)
        case r: FRAnimatedX3d => Util.fileCopy(r.modelFile, animatedModelsDir)
        case _ => // nothing to do
      }
    }
  }

  def toCarouselEntries(gaiaImage: GaiaImage, hpDir: Path, workDir: Path, tmpDir: Path, createResources: Boolean): Seq[CarouselEntry] = {
    val stillImageEntries = {
      val sgs = stillImageGroups(gaiaImage, workDir)
      sgs.map(sg => CarouselEntry(hpDir, sg.preview, FRImage(sg.full)))
    }
    val descImageEntry = {
      val outputImage: Path = tmpDir.resolve(s"desc-${gaiaImage.id}.png")
      if createResources then {
        descriptionImage(gaiaImage, outputImage, workDir)
      }
      Seq(CarouselEntry(hpDir, outputImage, FRNull))
    }

    val videoImageEntries = {
      gaiaImage.youTubeId.zipWithIndex.map { (videoUrl, i) =>
        val outputImage: Path = tmpDir.resolve(s"video-${gaiaImage.id}-$i.png")
        val logoName = "video-400.png"
        val logoFile = Path.of("src", "main", "html1", "res", logoName)
        if createResources then logoImage(gaiaImage, logoFile, outputImage, workDir)
        CarouselEntry(hpDir, outputImage, FRVideo(s"http://www.youtube.com/embed/$videoUrl"))
      }.toSeq
    }

    val x3dImageEntries = {
      def x3dCarouselEntry(x3dResource: Path): CarouselEntry = {
        val fname = x3dResource.getFileName.toString
        val name = fname.substring(0, fname.lastIndexOf('.'))
        val outImageName = s"x3d-$name.png"
        val outputImage: Path = tmpDir.resolve(outImageName)
        val logoFile = Path.of("src", "main", "html1", "res", "x3d-400.png")
        if createResources then logoImage(gaiaImage, logoFile, outputImage, workDir)
        CarouselEntry(hpDir, outputImage, FRX3d(x3dResource))
      }

      def res(dirName: String): Seq[Path] = {
        val mdir = Util.fileDirInOutDir(workDir, gaiaImage.id).resolve(dirName)
        if Files.notExists(mdir) then throw IllegalStateException(s"Directory 'models' missing for ${gaiaImage.id}")
        Files.list(mdir).toScala(Seq).filter(p => p.getFileName.toString.toLowerCase.endsWith("x3d"))
      }

      Seq("models", "animated-models")
        .flatMap(res)
        .map(x3dCarouselEntry)
    }

    descImageEntry ++ videoImageEntries ++ x3dImageEntries ++ Random.shuffle(stillImageEntries)
  }

  def createHp(workDir: Path, gaiaImages: Seq[GaiaImage], copyResources: Boolean): Unit = {
    val gaiaIds = gaiaImages.map(i => i.id)
    println(s"Creating HP for gaia images ${gaiaIds.mkString(",")}")

    ResourceCreator.createAll(gaiaIds, workDir)

    val hpDir = Util.fileDirInOutDir(workDir, "hp")
    if copyResources then Util.deleteDirContentRecursive(hpDir)

    def copyHtmlFiles(): Unit = {
      val names = Seq("gaia.css", "gaia.js", "vue-min.js", "Orbitron-VariableFont_wght.ttf")
      names.map(n => Path.of("src", "main", "html1", n))
        .foreach(p => Util.fileCopy(p, hpDir))
    }

    def carousellEntries(images: Seq[CarouselEntry]): String = {
      val imagesStr = images.map(e => e.toJson).mkString(",\n")
      s"""
         |{
         |     carousels: [
         |$imagesStr
         |     ]
         |}
         |""".stripMargin
    }

    def createHtml(carouselEntries: Seq[Seq[CarouselEntry]]): Unit = {
      val outfile = hpDir.resolve("index.html")
      val carouselsStr = carouselEntries.map(entry => carousellEntries(entry)).mkString(",\n")
      Util.writeString(outfile, htmlTemplate(carouselsStr))
    }

    Util.runWithTmpdir() { tmpDir =>
      val carouselEntries: Seq[Seq[CarouselEntry]] = gaiaImages
        .filter(i => i.hpOrder.isDefined)
        .sortBy(i => -i.hpOrder.get)
        .map(i => toCarouselEntries(i, hpDir, workDir, tmpDir, copyResources))

      if copyResources then carouselEntries.flatten.foreach(_.copyResources())
      createHtml(carouselEntries)
      copyHtmlFiles()
      println(s"Wrote hp to $hpDir")
    }
  }

  private def htmlTemplate(carouselsStr: String) = {
    s"""
       |<!DOCTYPE html>
       |<html lang="en">
       |<head>
       |    <meta charset="UTF-8">
       |    <title>gaia visual</title>
       |    <meta name="viewport" content="width=device-width, initial-scale=1">
       |    <link rel="stylesheet" href="gaia.css">
       |    <link rel="stylesheet" href="https://cdn.jsdelivr.net/npm/@mdi/font@5.8.55/css/materialdesignicons.min.css">
       |    <link rel="stylesheet" href="https://use.fontawesome.com/releases/v5.2.0/css/all.css">
       |</head>
       |<body>
       |<div id="app">
       |    <section class="hero is-primary">
       |        <div class="hero-body">
       |            <div class="container has-text-centered">
       |                <p class="title">
       |                    gaia visual
       |                </p>
       |                <p class="subtitle">
       |                    see what our galaxy looks like
       |                </p>
       |            </div>
       |        </div>
       |    </section>
       |    <div class="container">
       |        <b-table :data="images" :show-header="false"  :mobile-cards="false">
       |            <b-table-column field="image_name" label="Image" sortable v-slot="props">
       |                <b-carousel :autoplay="false"  :indicator="false" :has-drag="hasDrag" :arrow-hover="arrowHover">
       |                    <b-carousel-item @click.native="carousel_clicked(carousel, $$event)" v-for="(carousel, i) in props.row.carousels" :key="i">
       |                        <b-image v-if="carousel.entry_type === 'IMAGE'" :src="carousel.image_name" :alt="carousel.entry_type" ratio="16by9"></b-image>
       |                    </b-carousel-item>
       |                </b-carousel>
       |            </b-table-column>
       |        </b-table>
       |    </div>
       |</div>
       |
       |
       |<script src="vue-min.js"></script>
       |<script src="https://unpkg.com/buefy/dist/buefy.min.js"></script>
       |<script src="gaia.js"></script>
       |<script>
       |    let app = new Vue({
       |        data() {
       |            return {
       |                arrowHover: isMobile(),
       |                hasDrag: isMobile(),
       |                images: [
       |$carouselsStr
       |                ]
       |             }
       |         },
       |         methods: {
       |           carousel_clicked(dats, event) {
       |               if (event.detail == 1 && dats.image_name_full !== null) {
       |                  console.log('carousel_clicked image_name: ' + dats.image_name_full)
       |                  window.open(dats.image_name_full, "_self")
       |               }
       |            },
       |         }
       |    })
       |    app.$$mount('#app')
       |</script>
       |</body>
       |</html>
       |""".stripMargin
  }

  case class VidInfo(
                      path: Path,
                      durationSeconds: Double,
                      frameRate: Fraction,
                      sampleRate: Int,
                      width: Int,
                      height: Int,
                    )

  def vidInfo(videoPath: Path): VidInfo = {
    import net.bramp.ffmpeg.FFprobe
    import net.bramp.ffmpeg.probe.FFmpegFormat
    import net.bramp.ffmpeg.probe.FFmpegProbeResult
    import net.bramp.ffmpeg.probe.FFmpegStream

    val ffprobe = FFprobe("/usr/bin/ffprobe")
    val probeResult: FFmpegProbeResult = ffprobe.probe(videoPath.toString)
    val format = probeResult.getFormat
    val stre = probeResult.getStreams.get(0)
    VidInfo(
      path = videoPath,
      durationSeconds = format.duration,
      frameRate = stre.avg_frame_rate,
      sampleRate = stre.sample_rate,
      width = stre.width,
      height = stre.height
    )

  }

  def vidSlice(inVideo: Path, id: String, outDir: Path, durationRelative: Double = 0.3): Path = {
    println(s"slice video $inVideo")
    val duration = Hp1.vidInfo(inVideo).durationSeconds
    val cutDuration = duration * durationRelative
    val start = Random.nextDouble() * (duration - cutDuration)
    val outFile = outDir.resolve(s"a-$id.mp4")
    val cmd = Seq("ffmpeg", "-ss", start.toInt.toString, "-i", inVideo.toString, "-t", cutDuration.toInt.toString, "-c", "copy", outFile.toString)
    Util.runAllCommands(Seq(cmd))
    outFile
  }

  def descriptionImage(gaiaImage: GaiaImage, outputImage: Path, workPath: Path): Unit = {
    val backImage = Util.stillImageGroups(gaiaImage, workPath).head.preview
    val (w, h) = Util.fileImageSize(backImage)

    val resName = backImage.getFileName.toString
    val sc =
      s"""
         |body {margin-top: -8px}
         |.back {
         |    background-image: url(res/$resName); 
         |    background-size: contain; 
         |    background-repeat: 
         |    no-repeat; 
         |    height: 100vh
         |}
         |.text {
         |    float: left;
         |    margin-top: 1em;
         |    margin-left: 1em;
         |    padding:0.3em;
         |    max-width: 26em;
         |    background-color: rgba(0, 0, 0, 0.7);
         |    color: white;
         |    font-size: 60px
         |}
         |""".stripMargin
    val bc =
      s"""
         |<div class="back">
         | <div class="text" >${gaiaImage.text}</div></div>
         |""".stripMargin
    Util.createImageFromHtml(outputImage, styleContent = sc, bodyContent = bc, w, h, resources = Seq(backImage))
  }

  def logoImage(gaiaImage: GaiaImage, logoFile: Path, outputImage: Path, workPath: Path): Unit = {
    val backImage = Random.shuffle(Util.stillImageGroups(gaiaImage, workPath)).head.preview

    val (widthBackground, heightBackground) = Util.fileImageSize(backImage)
    val (widthLogo, heightLogo) = Util.fileImageSize(logoFile)

    val topLogo = (heightBackground.toDouble / 2.0 - heightLogo.toDouble / 2.0).toInt
    val leftLogo = (widthBackground.toDouble / 2.0 - widthLogo.toDouble / 2.0).toInt
    val logoName = logoFile.getFileName.toString

    val resName = backImage.getFileName.toString
    val sc =
      s"""
         |body {margin-top: -8px}
         |.back {
         |    background-image: url(res/$resName); 
         |    background-size: contain; 
         |    background-repeat: 
         |    no-repeat; 
         |    height: 100vh
         |}
         |.logo {
         |    float: left;
         |    margin-top: ${topLogo}px;
         |    margin-left: ${leftLogo}px;
         |    width: ${widthLogo}px;
         |    height: ${heightLogo}px;
         |    background-image: url(res/$logoName); 
         |}
         |""".stripMargin
    val bc =
      s"""
         |<div class="back">
         | <div class="logo" ></div></div>
         |""".stripMargin
    Util.createImageFromHtml(outputImage, styleContent = sc, bodyContent = bc, widthBackground, heightBackground, resources = Seq(backImage, logoFile), workPath = Some(workPath))
  }

  enum Transition(val id: String):
    case FADE extends Transition("fade")
    case WIPEUP extends Transition("wipeup")

  @tailrec
  def vidConcat(videos: List[Path], id: Int, workDir: Path,
                fadeDurationInSeconds: Int = 5,
                transition: Transition = Transition.FADE): Path = {

    def concat(a: Path, b: Path): Path = {
      val dur = Hp1.vidInfo(a).durationSeconds
      val output = workDir.resolve(s"concat-$id.mp4")
      val offs = (dur - (fadeDurationInSeconds + 2)).toInt
      val transId = transition.id
      val cmd = Seq(
        "ffmpeg", "-i", a.toString, "-i", b.toString,
        "-filter_complex", s"xfade=transition=$transId:offset=$offs:duration=6",
        output.toString)
      Util.runAllCommands(Seq(cmd))
      output
    }

    videos match {
      case Nil => throw IllegalStateException("No video nothing to concat")
      case a :: Nil => a
      case a :: b :: Nil => concat(a, b)
      case a :: b :: rest =>
        val c = concat(a, b)
        vidConcat(c :: rest, id + 1, workDir)
    }
  }


  object ResourceCreator {

    def createAll(ids: Seq[String], workDir: Path): Unit = {
      println("START Creating resources for HP")
      ids.foreach(create(_, workDir))
      println("END Creating resources for HP")
    }

    def create(id: String, workDir: Path): Unit = {
      println(s"Creating resources for $id")
      if !resourceExists(id, workDir, "models", "x3d") then
        Gaia.createX3d(List(id), workDir)
      if !resourceExists(id, workDir, "animated-models", "x3d") then
        Gaia.createX3dAnimation(List(id), workDir)
      if !resourceExists(id, workDir, "stills", "png") then
        Gaia.createStill(List(id), workDir)
    }

    def resourceExists(id: String, workDir: Path, dirName: String, extension: String): Boolean = {
      val modelDir = Util.fileDirInOutDir(workDir, id).resolve(dirName)
      if Files.exists(modelDir) then
        Files.list(modelDir).filter(_.getFileName.toString.toLowerCase.endsWith(extension)).toScala(Iterable).nonEmpty
      else
        false
    }

  }


}

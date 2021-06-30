package gaia

import java.nio.file.{Files, Path}

object Cred {

  import Gaia._
  import Cam._

  def createTxt(gaiaImage: GaiaImage, workPath: Path): Unit = {
    println("--------------------------------------------------------------------------------")
    println(gaiaImage.desc)
    println()
    if gaiaImage.textVal.isDefined then {
      println(gaiaImage.text)
      println()
    }
    if !gaiaImage.credits.references.isEmpty then {
      gaiaImage.credits.references.foreach(ref => println(ref))
      println()
    }
    println("--------------------------------------------------------------------------------")
  }

  def cssPercent(height: Int)(percent: Int): String = "%.0f".format(height * (percent.toDouble / 100))

  def createOpeningCredit(gaiaImage: GaiaImage, w: Int, h: Int, outDir: Path, workDir: Option[Path] = None): Path = {
    def cp = cssPercent(h)

    val styleContent =
      s"""        .title {
         |            font-size: ${cp(10)}px;
         |            font-weight: bold;
         |            text-align: center;
         |            margin-top: ${cp(30)}px;
         |            margin-bottom: ${cp(10)}px;
         |        }
         |        .subtitle {
         |            font-size: ${cp(3)}px;
         |            text-align: center;
         |            margin-left: 100px;
         |            margin-right: 100px;
         |        }
         |""".stripMargin

    val bodyContent =
      s"""<div class="title">GAIA VISUAL</div>
         |<div class="subtitle">${gaiaImage.text}</div>
         |""".stripMargin

    val imgFile = outDir.resolve(s"${s"opening_${gaiaImage.id}"}.png")
    Util.createImageFromHtml(imgFile, styleContent, bodyContent, w, h, workPath = workDir)
    imgFile
  }

  def createClosingCredit(gaiaImage: GaiaImage, w: Int, h: Int, outDir: Path): Path = {
    def cp = cssPercent(h)
    val references: Seq[String] = gaiaImage.credits.references
    val refs = references.map {
      ref =>
        s"""<div class="ref">
           |$ref
           |</div>
           |""".stripMargin
    }.mkString("\n")

    val styleContent =
      s"""        .refs {
         |            margin-top: ${cp(40)}px;
         |            margin-left: 100px;
         |            margin-right: 100px;
         |        }
         |        .ref {
         |            font-size: ${cp(3)}px;
         |            text-align: center;
         |        }
         |""".stripMargin

    val bodyContent =
      s"""<div class="refs">
         |$refs
         |</div>
         |""".stripMargin

    val imageFile = outDir.resolve(s"${s"closing_${gaiaImage.id}"}.png")
    Util.createImageFromHtml(imageFile, styleContent, bodyContent, w, h)
    imageFile
  }

  def create(gaiaImage: GaiaImage, workPath: Path): Unit = {
    val vq: VideoResolution = gaiaImage.videoQuality.videoResolution
    val w = vq.width
    val h = vq.height

    val imgPath = Util.fileDirInOutDir(workPath, gaiaImage.id)
    val creditsDir = Util.fileDirFromDir(imgPath, "credits")

    createOpeningCredit(gaiaImage, w, h, creditsDir)
    createClosingCredit(gaiaImage, w, h, creditsDir)
  }


}

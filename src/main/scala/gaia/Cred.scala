package gaia

import java.nio.file.{Files, Path}

object Cred {

  import Main._
  import Cam._

  def create(gaiaImage: GaiaImage, workPath: Path): Unit = {

    val vq: VideoQuality = gaiaImage.videoQuality
    val cvq = Cam.mapVideoQuality(vq)
    val (w, h) = cvq.geometry

    def hp(percent: Int): String = {
      "%.0f".format(h * (percent.toDouble / 100))
    }


    val imgPath = workPath.resolve(gaiaImage.id)
    val creditsDir = imgPath.resolve("credits")
    if Files.notExists(creditsDir) then Files.createDirectories(creditsDir)

    val txt = gaiaImage.text

    val openingContent =
      s"""<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <meta name="viewport" content="width=device-width, initial-scale=0.7">
         |    <meta name="theme-color" content="#000">
         |    <title>Gaia Visual</title>
         |    <link rel="stylesheet" href="css/gaia.css">
         |    <style>
         |        .title {
         |            font-size: ${hp(10)}px;
         |            font-weight: bold;
         |            text-align: center;
         |            margin-top: ${hp(30)}px;
         |            margin-bottom: ${hp(10)}px;
         |        }
         |        .subtitle {
         |            font-size: ${hp(3)}px;
         |            text-align: center;
         |            margin-left: 100px;
         |            margin-right: 100px;
         |        }
         |    </style>
         |</head>
         |<body>
         |<div class="title">GAIA VISUAL</div>
         |<div class="subtitle">$txt</div>
         |</body>
         |</html>
         |""".stripMargin

    val references: Seq[String] = gaiaImage.credits.references
    val refs = references.map {
      ref =>
        s"""<div class="ref">
           |$ref
           |</div>
           |""".stripMargin
    }.mkString("\n")

    val closingContent =
      s"""<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <meta name="viewport" content="width=device-width, initial-scale=0.7">
         |    <meta name="theme-color" content="#000">
         |    <title>Gaia Visual</title>
         |    <link rel="stylesheet" href="css/gaia.css">
         |    <style>
         |        .refs {
         |            margin-top: ${hp(40)}px;
         |            margin-left: 100px;
         |            margin-right: 100px;
         |        }
         |        .ref {
         |            font-size: ${hp(3)}px;
         |            text-align: center;
         |        }
         |    </style>
         |</head>
         |<body>
         |<div class="refs">
         |$refs
         |</div>
         |</body>
         |</html>
         |""".stripMargin

    val openingName = s"opening_${gaiaImage.id}"
    val closingName = s"closing_${gaiaImage.id}"

    val openingHtmlFile = creditsDir.resolve(s"${openingName}.html")
    val closingHtmlFile = creditsDir.resolve(s"${closingName}.html")

    val openingImageFile = creditsDir.resolve(s"${openingName}.png")
    val closingImageFile = creditsDir.resolve(s"${closingName}.png")

    Util.writeString(openingHtmlFile, openingContent)
    Util.writeString(closingHtmlFile, closingContent)

    Util.recursiveCopy(Path.of("src", "main", "html", "css"), creditsDir.resolve("css"))

    val openingCmd = Seq("google-chrome", "--headless", s"-window-size=$w,$h", s"--screenshot=${openingImageFile.toAbsolutePath}", s"${openingHtmlFile.toAbsolutePath}")
    val closingCmd = Seq("google-chrome", "--headless", s"-window-size=$w,$h", s"--screenshot=${closingImageFile.toAbsolutePath}", s"${closingHtmlFile.toAbsolutePath}")

    Util.runAllCommands(Seq(openingCmd, closingCmd))

    println(s"Creating credits for ${gaiaImage.id} in ${creditsDir.toAbsolutePath}")

  }

}

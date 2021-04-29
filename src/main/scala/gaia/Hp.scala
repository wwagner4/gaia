package gaia

import gaia.Gaia.{GaiaImage, createX3d, images}

import java.io.IOException
import java.nio.file.{Files, Path, StandardCopyOption}
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Hp {


  def createHp(args: List[String], workDir: Path): Unit = {
    val htmlDir = workDir.resolve("html")
    if (Files.notExists(htmlDir)) Files.createDirectories(htmlDir)
    val modelsDir = workDir.resolve("models")

    val imgs = images.values
      .toSeq
      .filter(i => i.hpOrder.isDefined)
      .sortBy(i => -i.hpOrder.get)

    imgs.foreach(i => createHtml(i, htmlDir))
    createIndexHtml(htmlDir, imgs)
    copyResources(htmlDir, modelsDir)
  }

  def createIndexHtml(htmlDir: Path, imgs: Seq[GaiaImage]) = {
    val gimages = imgs.map(gaiaImageToHtml).mkString("\n")

    val html =
      s"""
         |<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <meta name="viewport" content="width=device-width, initial-scale=0.7">
         |    <meta name="theme-color" content="#000">
         |    <title>Gaia Visual</title>
         |    <link rel="stylesheet" href="css/gaia.css">
         |</head>
         |<body>
         |<h1>GAIA VISUAL</h1>
         |<p>
         |    Visualisations of the data provided by the
         |    <a href="https://en.wikipedia.org/wiki/Gaia_(spacecraft)" TARGET="_blank"> gaia space observatory</a>,
         |    launched in 2013.
         |</p>
         |Usage
         |<table>
         |    <tr>
         |        <td>viewer-</td>
         |        <td>Download the x3d-model file and render it at your local
         |            <a href="https://www.web3d.org/x3d/content/examples/X3dResources.html">x3d-viewer</a></td>
         |    </tr>
         |    <tr>
         |        <td>image-</td>
         |        <td>Show a larger version of the image</td>
         |    </tr>
         |    <tr>
         |        <td>video-</td>
         |        <td>Show a video created by using the x3d-model</td>
         |    </tr>
         |    <tr>
         |        <td>browser-</td>
         |        <td>Render the x3d-model on your browser using <a href="https://www.x3dom.org/">x3dom</a></td>
         |    </tr>
         |</table>
         |
         |<br/>
         |<br/>
         |<br/>
         |<br/>
         |$gimages
         |<p style="max-width: 800px">
         |    This page is brought to you by
         |    <a href="http://entelijan.net" TARGET="_blank"> entelijan.net</a>
         |</body>
         |</html>
         |
         |""".stripMargin
    val fnam = htmlDir.resolve("index.html")
    Util.writeString(fnam, html)
  }

  def copyResources(htmlDir: Path, modelsDir: Path): Unit = {
    val projectHtmlDir = Path.of("src", "main", "html")
    Util.recursiveCopy(projectHtmlDir, htmlDir)
    val htmlModelsDir = htmlDir.resolve("models")
    Util.recursiveCopy(modelsDir, htmlModelsDir)
  }

  def createHtml(gaiaImage: GaiaImage, htmlPath: Path): Unit = {
    val gaiaImageId = gaiaImage.id
    val htmlFn = s"$gaiaImageId.html"
    val x3dFn = s"$gaiaImageId.x3d"
    val file = htmlPath.resolve(htmlFn)

    val html =
      s"""
         |<!DOCTYPE html>
         |<html lang="en">
         |<head>
         |    <meta charset="UTF-8">
         |    <title>Gaia $gaiaImageId</title>
         |    <script type='text/javascript' src='js/x3d.js'> </script>
         |    <link rel='stylesheet' type='text/css' href='css/x3d.css'/>
         |    <meta name="theme-color" content="#000">
         |</head>
         |<body>
         |<x3d >
         |    <scene>
         |        <inline url="models/$x3dFn" />
         |    </scene>
         |</x3d>
         |</body>
         |</html>""".stripMargin

    Util.writeString(file, html)
    println(s"created html ${file.toAbsolutePath}")
  }


  def gaiaImageToHtml(gaiaImage: GaiaImage): String = {
    val videoLink = gaiaImage.video match {
      case Some(url) => s"""<a href="${url}">video</a>"""
      case None => "video"
    }
    val browserLink =
      if (gaiaImage.renderWithBrowser) s"""<a href="${gaiaImage.id}.html">browser</a>"""
      else "browser"

    val links = Seq(
      s"""<a href="models/${gaiaImage.id}.x3d">viewer</a>""",
      s"""<a href="images/${gaiaImage.id}_full.jpg">image</a>""",
      videoLink,
      browserLink
    ).mkString(" | ")

    s"""
       |<div class="gimage">
       |<a href="images/${gaiaImage.id}_full.jpg"><img  class="topimage" src="images/${gaiaImage.id}_m.jpg" alt="i${gaiaImage.id}"/></a>
       |<p>
       |$links
       |</p>
       |<p class="text">${gaiaImage.text}</p>
       |</div>
       |""".stripMargin.trim
  }

}

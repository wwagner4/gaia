package gaia

import java.nio.file.{Files, Path}
import scala.collection.JavaConverters._
import scala.language.implicitConversions

object Hp {

  case class GaiaImage(
                        id: String,
                        renderWithBrowser: Boolean = false,
                        video: Option[String] = None,
                        order: Int = Int.MaxValue,
                        text: String = "No text provided",
                      )

  val gaiaImages = Seq(
    GaiaImage(
      id = "image1_osp",
      order = 10,
      renderWithBrowser = true,
      text =
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 27104 Stars which are visualized as points.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage(
      id = "image1_oss",
      order = 20,
      text =
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 2710 Stars which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage(
      id = "image1_shp",
      renderWithBrowser = true,
      order = 30,
      text =
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 4000, 8000 and 14000 Stars from the inner to the outer shell 
          |which are visualized as points.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage(
      id = "image1_shs",
      order = 40,
      video = Some("https://www.youtube.com/embed/hXMWpzVQsX8"),
      text =
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 1700, 2000 and 1000 Stars from the inner to the outer shell 
          |which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage(
      id = "image1_sp16",
      order = 50,
      renderWithBrowser = true,
      text = spText("16")
    ),
    GaiaImage(
      id = "image1_sp8",
      order = 60,
      renderWithBrowser = true,
      text = spText("8")
    ),
    GaiaImage(
      id = "image1_sp5",
      order = 70,
      renderWithBrowser = true,
      text = spText("5")
    ),
    GaiaImage(
      id = "image1_sp2",
      order = 80,
      renderWithBrowser = true,
      text = spText("2")
    ),
  )

  private def spText(dist: String) = {
    s"""Stars around the sun with a maximum distance of $dist kpc.
       |Some stars are filtered out to make the image clearer. 
       |Near the center more stars are filtered out than close to the edge
       |to avoid bulges around the sun.
       |""".stripMargin.trim
  }

  lazy val htmlPath = Util.htmlPath


  def createHp(id: String): Unit = {
    createHtmlsForX3d()
  }

  def gaiaImage(gaiaImageId: String): GaiaImage = {
    gaiaImages
      .map(gi => (gi.id, gi))
      .toMap
      .getOrElse(gaiaImageId, GaiaImage(id = gaiaImageId))
  }

  def createHtmlsForX3d(): Unit = {

    def createHtml(gaiaImage: GaiaImage): Unit = {
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

    val files = Files.list(Util.modelPath).iterator().asScala.toList
      .filter(p => p.getFileName.toString.endsWith("x3d"))
      .map(bareFilename)
      .map(id => gaiaImage(id))
      .sortBy(gi => -gi.order)

    files.foreach(createHtml(_))
    println("-------------------------------------------------------------")
    files.foreach { gi =>
      val line = gaiaImageToHtml(gi)
      println(s"$line")
    }
    println("-------------------------------------------------------------")

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
       |<a href="images/${gaiaImage.id}_full.jpg"><img src="images/${gaiaImage.id}_m.jpg" alt="i${gaiaImage.id}"/></a>
       |<p>
       |$links
       |</p>
       |<p class="text">${gaiaImage.text}</p>
       |</div>
       |""".stripMargin.trim
  }

  // TODO evtl Util
  private def bareFilename(path: Path): String = {
    val fnamStr = path.getFileName.toString
    val idx = fnamStr.lastIndexOf('.')
    fnamStr.substring(0, idx)
  }

}

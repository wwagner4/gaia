package gaia


import java.nio.file.{Files, Path}

object Automove {

  import Gaia._
  import Vector._

  enum Resolution(val width: Int, val height: Int) {

    def resString = s"${width}x${height}"

    case _4kWide extends Resolution(width = 4098, height = 2160)

    case _4k extends Resolution(width = 3840, height = 2160)

    case UltraHD extends Resolution(width = 2560, height = 1440)

    case FullHD extends Resolution(width = 1920, height = 1080)

    case HD extends Resolution(width = 1280, height = 720)

    case SVGA extends Resolution(width = 800, height = 600)

    case VGA extends Resolution(width = 640, height = 480)

  }

  enum FrameRate(perSecond: Int) {

    def seconds = {
      val sec = 1.0 / perSecond
      "%.3f".format(sec)
    }

    case _60 extends FrameRate(60)

    case _50 extends FrameRate(50)

    case _48 extends FrameRate(48)

    case _30 extends FrameRate(30)

    case _25 extends FrameRate(25)

    case _24 extends FrameRate(24)

  }

  case class RotAxesDeg(ra: Double, dec: Double) {
    def toVec: Vec = {
      PolarVec(1, degToRad(ra), degToRad(dec)).toVec
    }
  }


  object RotAxesDeg {
    def nearEcliptic = RotAxesDeg(87.3, 90)

    def steep = RotAxesDeg(33, 90)

    def aroundX = RotAxesDeg(0, 0)

    def aroundY = RotAxesDeg(0, 90)

    def aroundZ = RotAxesDeg(90, 0)
  }


  case class MoveConfig(
                         id: String,
                         viewpoint: Vec,
                         cycleInterval: Int,
                         rotation: RotAxesDeg,
                         rotationAxis: RotAxesDeg = RotAxesDeg.aroundX
                       ) extends Identifiable

  case class VideoConfig(
                          name: String,
                          references: Seq[String],
                          resolution: Resolution,
                          frameRate: FrameRate,
                          frameCount: Int,
                          moves: Seq[MoveConfig],
                        )


  override def hashCode(): Int = super.hashCode()

  def sunos2(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "One shell 8kpc",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2000,
      moves = Seq(
        MoveConfig(
          id = "01",
          viewpoint = Vec(0, 0, 10),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg.aroundX,
          rotation = RotAxesDeg(0, 10)
        ),
        MoveConfig(
          id = "02",
          viewpoint = Vec(0, 0, 20),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(0, 10)
        ),
        MoveConfig(
          id = "03",
          viewpoint = Vec(5, -1.5, 6),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(90, 70)
        ),
        MoveConfig(
          id = "04",
          viewpoint = Vec(0, 0, 25),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(180, 20)
        ),
      ),
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def sunms2(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Three shells",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2000,
      moves = Seq(
        MoveConfig(
          id = "01",
          viewpoint = Vec(2, 3, 10),
          cycleInterval = 80,
          rotationAxis = RotAxesDeg(90, 45),
          rotation = RotAxesDeg.aroundX
        ),
        MoveConfig(
          id = "02",
          viewpoint = Vec(0, 0, 20),
          cycleInterval = 80,
          rotationAxis = RotAxesDeg(-90, 45),
          rotation = RotAxesDeg.aroundY
        ),
        MoveConfig(
          id = "03",
          viewpoint = Vec(0, 0, 25),
          cycleInterval = 80,
          rotationAxis = RotAxesDeg.aroundX,
          rotation = RotAxesDeg.nearEcliptic
        ),
      ),
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def sunnear1(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Around the sun 2kpc",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2000,
      moves = Seq(
        MoveConfig(
          id = "01",
          viewpoint = Vec(0, 0, 3),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg(20, 34),
          rotation = RotAxesDeg.aroundZ
        ),
        MoveConfig(
          id = "02",
          viewpoint = Vec(0, 0, 5),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg(-20, 90),
          rotation = RotAxesDeg.aroundX
        ),
        MoveConfig(
          id = "03",
          viewpoint = Vec(0, 0, 2),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg(0, 190),
          rotation = RotAxesDeg.aroundX
        ),
      ),
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def sund27(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Around the sun 27pc",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2000,
      moves = Seq(
        MoveConfig(
          id = "nearEcliptic",
          viewpoint = Vec(0, 0, 0.01),
          cycleInterval = 60,
          rotation = RotAxesDeg.nearEcliptic
        ),
        MoveConfig(
          id = "steep",
          viewpoint = Vec(0, 0, 0.07),
          cycleInterval = 120,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg.steep
        )
      )
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def sund3(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Movements near the sun",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2000,
      moves = Seq(
        MoveConfig(
          id = "nearEcliptic",
          viewpoint = Vec(0, 0, 5),
          cycleInterval = 60,
          rotation = RotAxesDeg.nearEcliptic
        ),
        MoveConfig(
          id = "steep",
          viewpoint = Vec(0, 0, 3),
          cycleInterval = 60,
          rotationAxis = RotAxesDeg.aroundZ,
          rotation = RotAxesDeg.steep
        )
      )
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def sund5(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Directions in 3 shells",
      references = Seq(
        "Music Try The Other Door by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch/MUSIC_FOR_TV_FILM__GAMES_VOL1/Try_The_Other_Door",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2500,
      moves = Seq(
        MoveConfig(
          id = "01",
          viewpoint = Vec(0, 0, 30),
          cycleInterval = 200,
          rotationAxis = RotAxesDeg.aroundX,
          rotation = RotAxesDeg.nearEcliptic
        ),
        MoveConfig(
          id = "02",
          viewpoint = Vec(0, 0, 15),
          cycleInterval = 200,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(0, 0)
        ),
        MoveConfig(
          id = "03",
          viewpoint = Vec(2, 0, 8),
          cycleInterval = 200,
          rotationAxis = RotAxesDeg(80, 5),
          rotation = RotAxesDeg(90, 45)
        ),
      ),
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def sund6(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Stars as spheres",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2500,
      moves = Seq(
        MoveConfig(
          id = "01",
          viewpoint = Vec(0, 0, 20),
          cycleInterval = 60,
          rotationAxis = RotAxesDeg.aroundX,
          rotation = RotAxesDeg.nearEcliptic,
        ),
        MoveConfig(
          id = "02",
          viewpoint = Vec(0, -5, 30),
          cycleInterval = 60,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(0, 0),
        ),
        MoveConfig(
          id = "03",
          viewpoint = Vec(2, 0, 10),
          cycleInterval = 60,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(180, 190),
        ),
        MoveConfig(
          id = "04",
          viewpoint = Vec(0, 0, 50),
          cycleInterval = 60,
          rotationAxis = RotAxesDeg.aroundZ,
          rotation = RotAxesDeg(0, 0),
        ),
      )
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def dens1(gcfg: GaiaImage, workDir: Path, preview: Boolean = false): Unit = {
    val cfg = VideoConfig(
      name = "Stars density",
      references = Seq(
        "Music by Daniel Birch: https://freemusicarchive.org/music/Daniel_Birch",
        "More: http://entelijan.net/wolfi-hp/gaia/"
      ),
      resolution = Resolution._4k,
      frameRate = FrameRate._60,
      frameCount = 2500,
      moves = Seq(
        MoveConfig(
          id = "01",
          viewpoint = Vec(-2, 0, 20),
          cycleInterval = 100,
          rotationAxis = RotAxesDeg.aroundX,
          rotation = RotAxesDeg(20, 40),
        ),
        MoveConfig(
          id = "02",
          viewpoint = Vec(-2, 0, 10),
          cycleInterval = 60,
          rotationAxis = RotAxesDeg.aroundY,
          rotation = RotAxesDeg(110, 20),
        ),
        MoveConfig(
          id = "03",
          viewpoint = Vec(-1, 0, 25),
          cycleInterval = 80,
          rotationAxis = RotAxesDeg.aroundX,
          rotation = RotAxesDeg(120, 155),
        ),
      )
    )
    createAutomove(gcfg, workDir, cfg)
  }

  def createAutomove(gcfg: GaiaImage, workDir: Path, cfg: VideoConfig): Unit = {
    println(s"Automove: ${gcfg.id}")

    val commands = for (moveCfg <- cfg.moves) yield {
      val automoveDir = workDir.resolve("automove")
      if (Files.notExists(automoveDir)) Files.createDirectories(automoveDir)
      val modelsDir = workDir.resolve("models")
      val videosDir = workDir.resolve("videos")
      if (Files.notExists(videosDir)) Files.createDirectories(videosDir)
      val modelFile = modelsDir.resolve(s"${gcfg.id}.x3d").toAbsolutePath.toFile
      val rotAxes = moveCfg.rotationAxis.toVec
      val rot = moveCfg.rotation.toVec
      val content =
        s"""
           |<X3D  profile='Immersive' version='3.4' >
           |  <Scene>
           |    <WorldInfo title='${gcfg.id} ${moveCfg.id}'/>
           |    <Background skyColor='${gcfg.backColor.strNoComma}'/>
           |    <Viewpoint	position='${moveCfg.viewpoint.strNoComma}' description='gaia_vp'/>		
           |    <Transform rotation='1 0 0 ${rot.x}'>
           |     <Transform rotation='0 1 0 ${rot.y}'>
           |      <Transform rotation='0 0 1 ${rot.z}'>
           |       <Transform DEF='object'>
           |     	  <Inline bboxCenter='0 0 0' bboxSize='-1 -1 -1' url='"file://$modelFile"'/>
           |       </Transform>
           |      </Transform>
           |     </Transform>
           |    </Transform>
           |    <TimeSensor DEF='clock' cycleInterval='${moveCfg.cycleInterval}' loop='true' />
           |    <OrientationInterpolator DEF='spinThings' key='0 0.5 1' keyValue='${rotAxes.strNoComma} 0  ${rotAxes.strNoComma} 3.14159  ${rotAxes.strNoComma} 6.28317'/>
           |    <ROUTE fromNode='clock' fromField='fraction_changed' toNode='spinThings' toField='set_fraction'></ROUTE>
           |    <ROUTE fromNode='spinThings' fromField='value_changed' toNode='object' toField='rotation'></ROUTE>
           |  </Scene>
           |</X3D>
           |""".stripMargin
      val moveFileBase = f"automove_${gcfg.id}_${moveCfg.id}"
      val moveFileName = moveFileBase + ".x3d"
      val moveFile = automoveDir.resolve(moveFileName)
      Util.writeString(moveFile, content)

      val videoFileName = s"$moveFileBase.mp4"
      val videoFile = videosDir.resolve(videoFileName).toAbsolutePath.toString
      println(s"Wrote to ${moveFile.toAbsolutePath}")
      List(
        "view3dscene", s"${moveFile.toAbsolutePath}", "--geometry", s"${cfg.resolution.resString}",
        "--screenshot-range", "0", s"${cfg.frameRate.seconds}", s"${cfg.frameCount}", s"$videoFile")
    }
    val refStr = cfg.references.mkString("\n")
    println(
      s"""--video snipplets-------------------------------
         |${cfg.name}
         |
         |${gcfg.text}
         |
         |$refStr
         |------------------------------------------------
         |""".stripMargin)
    commands.map(l => l.mkString(" ")).foreach(println)
    createCredits(gcfg, cfg, workDir)

  }

  def createCredits(gaiaImage: GaiaImage, videoConfig: VideoConfig, workDir: Path): Unit = {

    val w = videoConfig.resolution.width
    val h = videoConfig.resolution.height

    def hp(percent: Int): String = {
      "%.0f".format(h * (percent.toDouble / 100))
    }

    println(s"Creating credits for ${gaiaImage.id}")
    val creditsDir = workDir.resolve("credits")
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

    val refs = videoConfig.references.map {
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

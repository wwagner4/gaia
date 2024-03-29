package gaia


import gaia.ImageUtil.testStars

import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random


object Main {

  import Data.Star
  import ImageUtil.{StarPosDir, basicStars, writeModelToFile, nearSunStars, toStarPosDir}
  import X3d.{Color, PolarVecDeg, Vec}

  lazy val workPath = getCreateWorkPath

  trait Identifiable {
    def id: String
  }

  case class RotAxesDeg(ra: Double, dec: Double) {
    def toVec: Vec = {
      PolarVecDeg(1, ra, dec).toRad.toVec
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


  case class Action(
                     id: String,
                     desc: String,
                     call: (args: List[String], workPath: Path) => Unit
                   ) extends Identifiable


  case class GaiaImage(
                        id: String,
                        desc: String,
                        fCreateModel: (id: String, workPath: Path) => Unit,
                        renderWithBrowser: Boolean = false,
                        video: Option[String] = None,
                        hpOrder: Int = Int.MaxValue,
                        text: String = "No text provided",
                        realCreatable: Boolean = true,
                        hpRelevant: Boolean = true,
                        videoConfig: Option[VideoConfig] = None,
                        backColor: Color = Color.black,
                      ) extends Identifiable

  val actions = identifiableToMap(Seq(
    Action("hp", "create homepage files and snipplets", gaia.Hp.createHp),
    Action("x3d", "create x3d files for an image", createX3d),
    Action("vidDry", "create video sniplets from an existing x3d model", createVideo(true)(_, _)),
    Action("vid", "create video sniplets from an existing x3d model", createVideo(false)(_, _)),
    Action("tryout", "Tryout something during development by callin doIt()", Tryout.doit),
  ))

  val images: Map[String, GaiaImage] = identifiableToMap(Seq(
    GaiaImage("gal", "The whole galaxy", writeModelToFile(s => s, basicStars)(Image1.oneShellSpheres),
      hpOrder = 20,
      video = Some("https://www.youtube.com/embed/jAuJPadoYvs"),
      backColor = Color.darkBlue,
      text =
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 2710 Stars which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage("oss", "one shell around the galactic center with spheres", writeModelToFile(s => s, basicStars)(Image1.oneShellSpheres),
      hpOrder = 20,
      video = Some("https://www.youtube.com/embed/jAuJPadoYvs"),
      backColor = Color.darkBlue,
      text =
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 2710 Stars which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage("osp", "one shell around the galactic center with points", writeModelToFile(s => s, basicStars)(Image1.oneShellPoints),
      hpOrder = 10,
      renderWithBrowser = true,
      backColor = Color.darkBlue,
      video = Some("https://www.youtube.com/embed/cEVH0IhlJ4Y"),
      text =
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 27104 Stars which are visualized as points.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      )),

    ),
    GaiaImage("shs", "multiple shells around the sun", writeModelToFile(s => s, basicStars)(Image1.shellsSphere),
      hpOrder = 40,
      video = Some("https://www.youtube.com/embed/irbUh9Y_Ifg"),
      backColor = Color.darkBlue,
      text =
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 1700, 2000 and 1000 Stars from the inner to the outer shell 
          |which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
    ),
    GaiaImage("shp", "multiple shells around the sun", writeModelToFile(s => s, basicStars)(Image1.shellsPoints),
      renderWithBrowser = true,
      hpOrder = 30,
      backColor = Color.darkBlue,
      text =
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 4000, 8000 and 14000 Stars from the inner to the outer shell 
          |which are visualized as points.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim,
      video = Some("https://www.youtube.com/embed/JelflHQSamo"),
      videoConfig = Some(VideoConfig(
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
      )),
    ),
    GaiaImage("sp2", "stars within a distance of 2 kpc to the sun", 
      writeModelToFile(s => s, basicStars)(Image1.sphere2),
      hpOrder = 80,
      renderWithBrowser = true,
      backColor = Color.black,
      video = Some("https://www.youtube.com/embed/lp-Y_jpYmnw"),
      text =
        """Stars around the sun with a maximum distance of 2 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      )),

    ),
    GaiaImage("sp5", "stars within a distance of 5 kpc to the sun", writeModelToFile(s => s, basicStars)(Image1.sphere5),
      hpOrder = 70,
      renderWithBrowser = true,
      backColor = Color.black,
      text =
        """Stars around the sun with a maximum distance of 5 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
    ),
    GaiaImage("sp8", "stars within a distance of 8 kpc to the sun", writeModelToFile(s => s, basicStars)(Image1.sphere8),
      hpOrder = 60,
      renderWithBrowser = true,
      video = Some("https://www.youtube.com/embed/LbW1O-GUPS8"),
      backColor = Color.black,
      text =
        """Stars around the sun with a maximum distance of 8 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
    ),
    GaiaImage("sp16", "stars within a distance of 16 kpc to the sun", writeModelToFile(s => s, basicStars)(Image1.sphere16),
      hpOrder = 50,
      renderWithBrowser = true,
      backColor = Color.black,
      text =
        """Stars around the sun with a maximum distance of 16 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
    ),
    GaiaImage("ntsd27", "direction of stars within a distance of 27 pc to sun", writeModelToFile(s => s, nearSunStars)(Image1.nearSunDirections27pc),
      hpOrder = 90, 
      backColor = Color.veryDarkGreen,
      video = Some("https://www.youtube.com/embed/JuK80k5m4vU"),
      text =
        """
          |Directions of the movement of stars around the sun. Maximum distance 27 pc
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      ))
    ),
    GaiaImage("ntsdvi", "direction and velocety of stars to a distace of 40 pc",  writeModelToFile(s => s, nearSunStars)(Image1.nearSunVeloInner), 
      hpOrder = 100,
      renderWithBrowser = true,
      backColor = Color.veryDarkGreen,
      text =
        """
          |Directions of the movement of stars in a distance up to 40 pc
          |""".stripMargin.trim
    ),
    GaiaImage("dirtest", "direction and velocety of stars to a distace of 40 pc", writeModelToFile(s => s, nearSunStars)(Image1.nearSunDirtest),
      hpOrder = 100,
      renderWithBrowser = true,
      backColor = Color.black,
      text = 
        """
          |Directions of the movement of stars in a distance up to 40 pc
          |""".stripMargin.trim
    ),
    GaiaImage("ntsdvo", "direction and velocety of stars of 45 pc distance", writeModelToFile(s => s, nearSunStars)(Image1.nearSunVeloOuter),
      hpOrder = 110,
      renderWithBrowser = true,
      video = Some("https://www.youtube.com/embed/hUqVxwHVTZg"),
      backColor = Color.veryDarkGreen,
      text =
        """
          |Directions of the movement of stars in a distance of about 45 pc
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      ))
    ),
    GaiaImage("dir01", "direction and velocety of stars  8 kpc from the sun", writeModelToFile(toStarPosDir, basicStars)(Image1.dir01),
      hpOrder = 120,
      video = Some("https://www.youtube.com/embed/bZ0KkVM-Kwc"),
      renderWithBrowser = true,
      backColor = Color.veryDarkBlue,
      text =
        """
          |Movement of stars in a distance of about 8 kpc from the sun
          |""".stripMargin.trim
    ),
    GaiaImage("gc", "galactic circle", writeModelToFile(toStarPosDir, (p: Path) => Seq.empty[Star])(Image1.gc),
      hpOrder = 130,
      video = Some("https://www.youtube.com/embed/bZ0KkVM-Kwc"),
      renderWithBrowser = true,
      backColor = Color.veryDarkRed,
      text =
        """
          |Movement of stars in a distance of about 8 kpc from the sun
          |""".stripMargin.trim,
      hpRelevant = false,
    ),
    GaiaImage(id = "dir02",
      desc = "direction and velocety of stars arond the sun",
      fCreateModel = writeModelToFile(toStarPosDir, basicStars)(Image1.dir02),
      hpOrder = 140,
      renderWithBrowser = true,
      video = Some("https://www.youtube.com/embed/NWRHYBLjFv0"),
      backColor = Color.veryDarkBlue,
      text =
        """
          |Movement of stars around the sun in 3 shells. 
          |Crosshairs indicate the sun and the center of the galaxy
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      )),
    ),
    GaiaImage(id = "dirs",
      desc = "stars as spheres with direction color coded. 8 to 23 kpc",
      fCreateModel = writeModelToFile(toStarPosDir, basicStars)(Image1.dirs),
      hpOrder = 150,
      backColor = Color.black,
      video = Some("https://www.youtube.com/embed/j1GaECAYAi8"),
      text =
        """
          |Movement of stars around the sun in a distance between 8 and 23 kpc.
          |Crosshairs indicate the sun and the center of the galaxy
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      ))
    ),
    GaiaImage(id = "agc",
      desc = "around the galactic center",
      fCreateModel = writeModelToFile(s => s, basicStars)(Image2.aroundGalacticCenterSpheres),
      backColor = Color.veryDarkBlue,
      text =
        """Stars around the galactic center
          |""".stripMargin.trim,
      hpRelevant = false,
    ),
    GaiaImage(id = "agcd",
      desc = "around the galactic center",
      fCreateModel = writeModelToFile(ImageUtil.toStarPosDirGalactic, basicStars)(Image2.aroundGalacticCenterDirections),
      backColor = Color.veryDarkBlue,
      text =
        """Stars around the galactic center
          |""".stripMargin.trim,
      hpRelevant = false,
    ),
    GaiaImage(id = "dens",
      desc = "density of stars as shown by gaia",
      fCreateModel = writeModelToFile(ImageUtil.toStarPosDirGalactic, basicStars)(Image2.dens),
      hpOrder = 170,
      backColor = Color.veryDarkBlue,
      video = Some("https://www.youtube.com/embed/FMIKp63XT1U"),
      text =
        """Density of stars seen by gaia in a cube around the center of 
          |galaxy. The side of that cube is 16 kpc. 
          |Regions with many stars are marked as green bowls.
          |The center of the galaxy is marked with a crosshair.
          |""".stripMargin.trim,
      videoConfig = Some(VideoConfig(
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
      ))
    ),
    GaiaImage(id = "td1",
      desc = "test stars for checkin direction",
      fCreateModel = writeModelToFile(s => s, testStars)(Image2.testDir),
      backColor = Color.veryDarkBlue,
      text =
        """Configuration for testing the display of direction""".stripMargin.trim,
    ),
  ))

  private def usage(message: Option[String]) = {
    val drawingIds = actions.values.map(d => f"  ${d.id}%7s | ${d.desc}").mkString("\n")
    val messageString = message.map(m => "\nERROR: " + m + "\n").getOrElse("")
    val msg =
      s"""$messageString
         |usage: <actionID> [<actionParam>]
         |
         |actionId    ... Identifies an action.
         |actionParam ... Parameter for an action
         |
         |actionId  | description
         |----------|-----------------------------------------
         |$drawingIds
         |""".stripMargin
    println(msg)
  }

  def main(args: Array[String]): Unit = {
    def rest(args: Iterable[String]): List[String] = args.toList match {
      case Nil => List.empty[String]
      case _ :: rest => rest
    }

    val bgColor = X3d.Color.darkBlue
    if (args.length >= 1) {
      val actionId = args(0)
      try {
        println(s"Run $actionId. Workdir ${workPath.toAbsolutePath}")
        actions.get(actionId).map(c => c.call(rest(args), workPath)).getOrElse(usage(Some(s"Illegal Action ID $actionId")))
      } catch {
        case (e: IllegalArgumentException) => usage(Some(e.getMessage))
      }
    } else
      usage(Some("You must define an Action-ID"))
  }

  /**
   * Create an x3d model by calling the fCreateMethod of the image definition
   */
  private def createX3d(args: List[String], workPath: Path): Unit = {
    val info = "Valid IDs: " + images.values.map(i => i.id).mkString(", ")
    if (args.size < 1) throw new IllegalArgumentException(s"Define an ID for creating an x3d file. $info")
    val id = args(0)
    images.get(id) match {
      case None => throw new IllegalArgumentException(s"Unknown ID $id for creating an x3d file. $info")
      case Some(gaiaImage) =>
        println(s"Creating gaia x3d for ID ${gaiaImage.id}. ${gaiaImage.desc}")
        gaiaImage.fCreateModel(id, workPath)
    }
  }

  /**
   * Creates a video by passing the imige definition to the Automove method createAutomove
   */
  private def createVideo(isDry: Boolean)(args: List[String], workPath: Path): Unit = {
    val info = "Valid IDs: " + images.values.filter(i => i.videoConfig.isDefined).map(i => i.id).mkString(", ")
    if (args.size < 1) throw new IllegalArgumentException(s"Define an ID for creating videos. $info")
    val id = args(0)
    images.get(id) match {
      case None => throw new IllegalArgumentException(s"Unknown ID $id for creating videos. $info")
      case Some(gaiaImage) =>
        println(s"Creating a video for ID ${gaiaImage.id}. ${gaiaImage.desc}")
        Automove.createAutomove(isDry)(gaiaImage, workPath)
    }
  }

  private def identifiableToMap[T <: Identifiable](identifables: Seq[T]): Map[String, T] = {
    identifables.map(i => (i.id, i)).toMap
  }

  private def getCreateWorkPath: Path = {

    def workFromBase(base: Path): Path = {
      if (Files.notExists(base)) Files.createDirectories(base)
      val work = base.resolve("gaia")
      if (Files.notExists(work)) Files.createDirectories(work)
      work
    }

    val env = System.getenv("GAIA_WORK_BASE")
    if (env != null && !env.isEmpty) {
      val base = Path.of(env)
      workFromBase(base)
    } else {
      val home = Path.of(System.getProperty("user.home"))
      val base = home.resolve("work")
      workFromBase(base)
    }
  }

}



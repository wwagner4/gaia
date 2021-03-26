package gaia


import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random


object Main {

  import Data.Star
  import ImageUtil._
  import X3d.{Color}
  import Vector._
  import gaia.Image2._

  lazy val workPath = getCreateWorkPath

  trait Identifiable {
    def id: String
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
                        video: Option[String] = None,
                        hpOrder: Option[Int] = None,
                        textVal: Option[String] = None,
                        realCreatable: Boolean = true,
                        hpRelevant: Boolean = true,
                        videoConfig: Option[VideoConfig] = None,
                        backColor: Color = Color.black,
                      ) extends Identifiable {
    def text: String = if (textVal.isDefined) textVal.get else desc

    def renderWithBrowser: Boolean = hpOrder.isDefined
  }

  val actions = identifiableToMap(Seq(
    Action("hp", "create homepage files and snipplets", gaia.Hp.createHp),
    Action("x3d", "create x3d files for an image", createX3d),
    Action("vidDry", "create video sniplets from an existing x3d model", createVideo(true)(_, _)),
    Action("vid", "create video sniplets from an existing x3d model", createVideo(false)(_, _)),
    Action("tryout", "Tryout something during development by callin doIt()", Tryout.doit),
  ))

  val images: Map[String, GaiaImage] = identifiableToMap(Seq(
    GaiaImage("sunos1", "One shell around the sun. Stars as spheres",
      writeModelToFile(StarCollections.basicStars)(Image1.oneShellSpheres),
      hpOrder = Some(20),
      video = Some("https://www.youtube.com/embed/jAuJPadoYvs"),
      backColor = Color.darkBlue,
      textVal = Some(
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 2710 Stars which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sunos2", "One shell around the sun. Stars as points",
      writeModelToFile(StarCollections.basicStars)(Image1.oneShellPoints),
      hpOrder = Some(10),
      backColor = Color.darkBlue,
      video = Some("https://www.youtube.com/embed/cEVH0IhlJ4Y"),
      textVal = Some(
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 27104 Stars which are visualized as points.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      ),
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
    GaiaImage("sunms1", "Multiple shells around the sun. Stars as spheres",
      writeModelToFile(StarCollections.basicStars)(Image1.shellsSphere),
      hpOrder = Some(40),
      video = Some("https://www.youtube.com/embed/irbUh9Y_Ifg"),
      backColor = Color.darkBlue,
      textVal = Some(
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 1700, 2000 and 1000 Stars from the inner to the outer shell 
          |which are visualized as spheres.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sunms2", "Multiple shells around the sun. Stars as points",
      writeModelToFile(StarCollections.basicStars)(Image1.shellsPoints),
      hpOrder = Some(30),
      backColor = Color.darkBlue,
      textVal = Some(
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 4000, 8000 and 14000 Stars from the inner to the outer shell 
          |which are visualized as points.  
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      ),
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
    GaiaImage("sunnear1", "Stars near the sun (2kpc). Stars as spheres",
      writeModelToFile(StarCollections.basicStars)(Image1.sphere2),
      hpOrder = Some(80),
      backColor = Color.black,
      video = Some("https://www.youtube.com/embed/lp-Y_jpYmnw"),
      textVal = Some(
        """Stars around the sun with a maximum distance of 2 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
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
    GaiaImage("sunnear2", "Stars near thes sun (5kpc). Stars as spheres",
      writeModelToFile(StarCollections.basicStars)(Image1.sphere5),
      hpOrder = Some(70),
      backColor = Color.black,
      textVal = Some(
        """Stars around the sun with a maximum distance of 5 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sunnear3", "stars within a distance of 8 kpc to the sun",
      writeModelToFile(StarCollections.basicStars)(Image1.sphere8),
      hpOrder = Some(60),
      video = Some("https://www.youtube.com/embed/LbW1O-GUPS8"),
      backColor = Color.black,
      textVal = Some(
        """Stars around the sun with a maximum distance of 8 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sun16", "stars within a distance of 16 kpc to the sun",
      writeModelToFile(StarCollections.basicStars)(Image1.sphere16),
      hpOrder = Some(50),
      backColor = Color.black,
      textVal = Some(
        """Stars around the sun with a maximum distance of 16 kpc.
          |Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sund27", "direction of stars within a distance of 27 pc to sun",
      writeModelToFile(StarCollections.nearSunStars)(Image1.nearSunDirections27pc),
      hpOrder = Some(90),
      backColor = Color.veryDarkGreen,
      video = Some("https://www.youtube.com/embed/JuK80k5m4vU"),
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
    GaiaImage("sund1", "direction and velocety of stars to a distace of 40 pc",
      writeModelToFile(StarCollections.nearSunStars)(Image1.sund1),
      hpOrder = Some(100),
      backColor = Color.veryDarkGreen,
    ),
    GaiaImage("sund2", "direction and velocety of stars in shell with distance 40 pc",
      writeModelToFile(StarCollections.nearSunStars)(Image1.sund2),
      hpOrder = Some(105),
      backColor = Color.black,
    ),
    GaiaImage("sund3", "direction and velocety of stars of 45 pc distance",
      writeModelToFile(StarCollections.nearSunStars)(Image1.sund3),
      hpOrder = Some(110),
      video = Some("https://www.youtube.com/embed/hUqVxwHVTZg"),
      backColor = Color.veryDarkGreen,
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
    GaiaImage("sund4", "direction and velocety of stars  8 kpc from the sun",
      writeModelToFile(StarCollections.basicStars)(Image1.sund4),
      hpOrder = Some(120),
      video = Some("https://www.youtube.com/embed/bZ0KkVM-Kwc"),
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage(id = "sund5",
      desc = "direction and velocety of stars arond the sun",
      fCreateModel = writeModelToFile(StarCollections.basicStars)(Image1.sund5),
      hpOrder = Some(140),
      video = Some("https://www.youtube.com/embed/NWRHYBLjFv0"),
      backColor = Color.veryDarkBlue,
      textVal = Some(
        """
          |Movement of stars around the sun in 3 shells. 
          |Crosshairs indicate the sun and the center of the galaxy
          |""".stripMargin.trim
      ),
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
    GaiaImage(id = "sund6",
      desc = "stars as spheres with direction color coded. 8 to 23 kpc",
      fCreateModel = writeModelToFile(StarCollections.basicStars)(Image1.sund6),
      hpOrder = Some(150),
      backColor = Color.black,
      video = Some("https://www.youtube.com/embed/j1GaECAYAi8"),
      textVal = Some(
        """
          |Movement of stars around the sun in a distance between 8 and 23 kpc.
          |Crosshairs indicate the sun and the center of the galaxy
          |""".stripMargin.trim
      ),
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
    GaiaImage(id = "gc1",
      desc = "around the galactic center",
      fCreateModel = writeModelToFile(StarCollections.basicStars)(gc1),
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage(id = "gcd1",
      desc = "around the galactic center",
      fCreateModel = writeModelToFile(StarCollections.basicStars)(Image2.gcd1),
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage(id = "gcd2",
      desc = "around the galactic center",
      fCreateModel = writeModelToFile(StarCollections.basicStars)(gcd2),
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage(id = "dens1",
      desc = "density of stars as shown by gaia",
      fCreateModel = writeModelToFile(StarCollections.basicStars)(Image2.dens),
      hpOrder = Some(170),
      backColor = Color.veryDarkBlue,
      video = Some("https://www.youtube.com/embed/FMIKp63XT1U"),
      textVal = Some(
        """Density of stars seen by gaia in a cube around the center of 
          |galaxy. The side of that cube is 16 kpc. 
          |Regions with many stars are marked as green bowls.
          |The center of the galaxy is marked with a crosshair.
          |""".stripMargin.trim
      ),
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
    GaiaImage(id = "tspikes",
      desc = "test spikes",
      fCreateModel = writeModelToFile(StarCollections.Test.Cones.spikes)(Image2.shapesCyl),
      backColor = Color.veryDarkGreen,
    ),
    GaiaImage(id = "tccones",
      desc = "test cones of cones",
      fCreateModel = writeModelToFile(StarCollections.Test.Cones.sparse)(Image2.shapesCyl),
      backColor = Color.veryDarkRed,
    ),
    GaiaImage(id = "tptoc",
      desc = "test for polar to carttesian conversion",
      fCreateModel = writeModelToFile(StarCollections.Test.polarToCartTest)(Image2.ptoc),
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage(id = "tctop",
      desc = "test for cartesian to polar conversion",
      fCreateModel = writeModelToFile(StarCollections.Test.cartToPolarTest)(Image2.ptoc),
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage("info", "Info about the coordinates and distance in the galaxy",
      writeModelToFile((p: Path) => Seq.empty[Star])(Image1.gc),
      video = Some("https://www.youtube.com/embed/bZ0KkVM-Kwc"),
      backColor = Color.veryDarkRed,
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
    val imagesInfo = images.values.toSeq.sortBy(_.id)
    val infoList = imagesInfo.map(i => f"${i.id}%15s | ${i.desc}").mkString("\n")
    val info = "Define an ID for creating an x3d file:\n" + infoList
    if (args.size < 1) throw new IllegalArgumentException(info)
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



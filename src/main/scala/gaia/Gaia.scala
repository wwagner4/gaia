package gaia

import gaia.Gaia.workPath

import java.nio.file.Files.{createDirectories, notExists}
import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random


object Gaia {

  import Data.Star
  import ImageUtil._
  import X3d._
  import Vector._

  lazy val workPath: Path = getCreateWorkPath

  enum VideoResolution(val width: Int, val height: Int):
    case VGA extends VideoResolution(640, 480)
    case SVGA extends VideoResolution(800, 600)
    case HD extends VideoResolution(1200, 720)
    case FullHD extends VideoResolution(1920, 1080)
    case UltraHD extends VideoResolution(2560, 1440)
    case _4k extends VideoResolution(3840, 2160)
    case _4kwide extends VideoResolution(4098, 2160)

  case class VideoQuality(
                           videoResolution: VideoResolution,
                           antiAliasing: Int,
                           frameRate: Int,
                         )

  object VideoQuality {
    def default = VideoQuality(VideoResolution._4k, antiAliasing = 1, frameRate = 60)

    def defaultPreview = VideoQuality(VideoResolution.SVGA, antiAliasing = 3, frameRate = 5)
  }


  trait Identifiable {
    def id: String
  }

  case class Action(
                     id: String,
                     desc: String,
                     call: (args: List[String], workPath: Path) => Unit
                   ) extends Identifiable


  type FuncCreate = (gaiaImage: GaiaImage, workDir: Path, preview: Boolean) => Unit

  case class Camera(
                     name: String,
                     pos: Vec,
                     dir: Vec,
                     time: Int,
                   )

  type FCams = Int => Seq[Camera]

  case class CameraConfig(
                           id: String,
                           cams: FCams,
                           durationInSec: Int,
                           modelRotation: Rotation1 = Rotation1(0, 0, 0),
                         )


  enum VideoConfig {
    case Call(fVideo: FuncCreate) extends VideoConfig
    case Cams(camConfigs: Seq[CameraConfig]) extends VideoConfig
  }


  case class CreditConfig(
                           references: Seq[String] = Seq(),
                         )

  case class StillConfig(
                          fStill: FuncCreate,
                        )

  case class GaiaImage(
                        id: String,
                        desc: String,
                        fCreateModel: (workPath: Path, backColor: Color) => Seq[Shapable],
                        video: Option[String] = None,
                        hpOrder: Option[Int] = None,
                        textVal: Option[String] = None,
                        realCreatable: Boolean = true,
                        videoConfig: Option[VideoConfig] = None,
                        backColor: Color = Color.black,
                        videoQuality: VideoQuality = VideoQuality.default,
                        credits: CreditConfig = CreditConfig(),
                        stillImageSeed: Long = 2348948509348L
                      ) extends Identifiable {
    def text: String = if (textVal.isDefined) textVal.get else desc

    def renderWithBrowser: Boolean = hpOrder.isDefined
  }

  val actions: Map[String, Action] = identifiableToMap(Seq(
    Action("hp", "create homepage files and snipplets", gaia.Hp.createHp),
    Action("x3d", "create x3d files for an image", createX3d),
    Action("x3da", "create x3d animation file files for an image", createX3dAnimation),
    Action("vid", "create video sniplets from ax3d model", createVideo),
    Action("vidp", "create preview video sniplets from a x3d model", createPreviewVideo),
    Action("still", "create still images from a x3d model", createStill),
    Action("cred", "create credits", createCredits),
    Action("credtxt", "create credits", createCreditsTxt),
    Action("tryout", "Tryout something during development by calling 'doIt' in Tryout", Tryout.doit),
  ))

  val images: Map[String, GaiaImage] = identifiableToMap(Seq(
    GaiaImage("sunos1", "One shell around the sun. Stars as spheres",
      ImageFactory.sunos1,
      hpOrder = Some(20),
      video = Some("https://www.youtube.com/embed/jAuJPadoYvs"),
      backColor = Color.darkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("far", Cam.cameras(90, 20, 15), 130),
        CameraConfig("ecc", Cam.cameras(120, -30, 15, eccentricity = 0.8, offset = Vec(0, 0, 5)), 130),
        CameraConfig("aeq", Cam.cameras(0, 60, 18, eccentricity = 0.7, offset = Vec(3, 0, 3)), 130),
      ))),
      textVal = Some(
        """One shell around the sun between 7 and 9 kpc.
          |The shell contains 2710 Stars which are visualized as spheres.
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sunos2", "One shell around the sun. Stars as points",
      ImageFactory.sunos2,
      hpOrder = Some(10),
      backColor = Color.darkBlue,
      video = Some("https://www.youtube.com/embed/cEVH0IhlJ4Y"),
      textVal = Some(
        """One shell around the sun between 7 and 9 kpc.
          |The shell contains 27104 Stars which are visualized as points.
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("aqu", Cam.cameras(0, 15, 15, eccentricity = 0.7), 100, rot(y = 60)),
        CameraConfig("pol", Cam.cameras(100, 80, 22, eccentricity = 0.5), 100)
      ))),
    ),
    GaiaImage("sunms1", "Multiple shells around the sun. Stars as spheres",
      ImageFactory.sunms1,
      hpOrder = Some(40),
      video = Some("https://www.youtube.com/embed/irbUh9Y_Ifg"),
      backColor = Color.darkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("simple",
          Cam.cameras(0, 15, 25, eccentricity = 0.7, offset = Vec(0, -10, -3)),
          durationInSec = 120),
        CameraConfig("gc",
          Cam.cameras(0, 15, 20, center = Vec(0, -8, 0)),
          durationInSec = 120),
        CameraConfig("ecc",
          Cam.cameras(90, 30, 25, center = Vec(0, -4, 0), eccentricity = 0.9, reverse = true),
          durationInSec = 120,
          modelRotation = rot(0, 70, 0)),
      ))),
      textVal = Some(
        """Three shells around the sun with distances 5, 8 and 11 kpc.
          |The shells contains 1700, 2000 and 1000 Stars from the inner to the outer shell
          |which are visualized as spheres.
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      )
    ),
    GaiaImage("sunms2", "Multiple shells around the sun. Stars as points",
      ImageFactory.sunms2,
      hpOrder = Some(30),
      backColor = Color.darkBlue,
      textVal = Some(
        """Three shells around the sun with distances 5, 8 and 11 kpc.
          |The shells contains 4000, 8000 and 14000 Stars from the inner to the outer shell
          |which are visualized as points.
          |The sun and the galactic center is displayed as crosshairs.
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("simple",
          Cam.cameras(0, 15, 25, eccentricity = 0.0, offset = Vec(0, 0, 0)),
          durationInSec = 60),
      ))),
      video = Some("https://www.youtube.com/embed/JelflHQSamo"),
    ),
    GaiaImage("sunnear1", "Stars near the sun (2kpc). Stars as spheres",
      ImageFactory.sunnear1,
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
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("far",
          Cam.cameras(45, 15, 5, eccentricity = 0.5, offset = Vec(0, 0.4, 0)),
          modelRotation = rot(y = 60),
          durationInSec = 60),
        CameraConfig("near",
          Cam.cameras(45, -40, 3, eccentricity = 0.9, offset = Vec(1, 0.4, 0)),
          modelRotation = rot(y = 60),
          durationInSec = 60),
      ))),
    ),
    GaiaImage("sunnear2", "Stars near thes sun (5kpc). Stars as spheres",
      ImageFactory.sunnear2,
      hpOrder = Some(70),
      backColor = Color.black,
      textVal = Some(
        """Stars around the sun with a maximum distance of 5 kpc.
          |Some stars are filtered out to make the image clearer.
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 7, eccentricity = 0.7, offset = Vec(1, 0, 0)),
          durationInSec = 60),
      ))),
    ),
    GaiaImage("sunnear3", "stars within a distance of 8 kpc to the sun",
      ImageFactory.sunnear3,
      hpOrder = Some(60),
      video = Some("https://www.youtube.com/embed/LbW1O-GUPS8"),
      backColor = Color.black,
      textVal = Some(
        """Stars around the sun with a maximum distance of 8 kpc.
          |Some stars are filtered out to make the image clearer.
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 7, eccentricity = 0.7, offset = Vec(1, 0, 0)),
          durationInSec = 60),
        CameraConfig("b",
          Cam.cameras(20, -50, 9, eccentricity = 0.9, offset = Vec(1, 1, 0)),
          durationInSec = 60),
        CameraConfig("c",
          Cam.cameras(-60, -5, 15, eccentricity = 0.5, offset = Vec(0, 0, 0)),
          modelRotation = rot(x = 7, z = 50),
          durationInSec = 60),
      ))),
    ),
    GaiaImage("sun16", "stars within a distance of 16 kpc to the sun",
      ImageFactory.sun16,
      hpOrder = Some(50),
      backColor = Color.black,
      textVal = Some(
        """Stars around the sun with a maximum distance of 16 kpc.
          |Some stars are filtered out to make the image clearer.
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 7, eccentricity = 0.7, offset = Vec(1, 0, 0)),
          durationInSec = 60),
      ))),
    ),
    GaiaImage("sund27", "direction of stars within a distance of 27 pc to sun",
      ImageFactory.sund27,
      hpOrder = Some(90),
      backColor = Color.veryDarkGreen,
      video = Some("https://www.youtube.com/embed/JuK80k5m4vU"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 7, eccentricity = 0.7, offset = Vec(1, 0, 0)),
          durationInSec = 60),
      ))),
    ),
    GaiaImage(id = "sund1",
      desc = "around the sun 40",
      textVal = Some(
        """direction and velocety of stars up to a distace of 40 pc
          |""".stripMargin),
      fCreateModel = ImageFactory.sund1,
      video = Some(" https://youtu.be/AZaBZWo0uwQ"),
      hpOrder = Some(100),
      backColor = Color.veryDarkGreen,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("near", Cam.cameras(0, 20, 0.05), 60),
        CameraConfig("far", Cam.cameras(0, -45, 0.1, reverse = true), 60),
      ))),
      credits = CreditConfig(references = Seq(
        "creation: entelijan",
        "http://entelijan.net",
        "music: Daniel Birch",
        "https://freemusicarchive.org/music/Daniel_Birch",
      )),
    ),
    GaiaImage("sund2", "direction and velocety of stars in shell with distance 40 pc",
      ImageFactory.sund2,
      hpOrder = Some(105),
      backColor = Color.black,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 80, 0.1), 60),
        CameraConfig("b", Cam.cameras(0, 20, 0.2, eccentricity = 0.99), 60),
      ))),
    ),
    GaiaImage("sund3", "direction and velocety of stars of 45 pc distance",
      fCreateModel = ImageFactory.sund3,
      hpOrder = Some(110),
      video = Some("https://www.youtube.com/embed/hUqVxwHVTZg"),
      backColor = Color.veryDarkGreen,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 40, 0.08), 60),
        CameraConfig("b", Cam.cameras(33, 60, 0.12, eccentricity = 0.8), modelRotation = rot(y = 45), durationInSec = 60),
        CameraConfig("c", Cam.cameras(99, 20, 0.2, eccentricity = 0.999), modelRotation = rot(y = -45), durationInSec = 60),
      ))),
    ),
    GaiaImage("sund4", "direction and velocety of stars  8 kpc from the sun",
      ImageFactory.sund4,
      hpOrder = Some(120),
      video = Some("https://www.youtube.com/embed/bZ0KkVM-Kwc"),
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 60, 20, eccentricity = 0.9), 120),
        CameraConfig("b", Cam.cameras(90, -40, 20, eccentricity = 0.6, center = Vec(0, -5, -1)), modelRotation = rot(z = 20), durationInSec = 120),
        CameraConfig("c", Cam.cameras(0, 5, 15, offset = Vec(0, 0, -10)), modelRotation = rot(y = 30), durationInSec = 120),
      ))),
    ),
    GaiaImage(id = "sund5",
      desc = "direction and velocety of stars arond the sun",
      fCreateModel = ImageFactory.sund5,
      hpOrder = Some(140),
      video = Some("https://www.youtube.com/embed/NWRHYBLjFv0"),
      backColor = Color.veryDarkBlue,
      textVal = Some(
        """
          |Movement of stars around the sun in 3 shells.
          |Crosshairs indicate the sun and the center of the galaxy
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 6, 20, eccentricity = 0.1), 130),
        CameraConfig("b", Cam.cameras(90, -40, 15, eccentricity = 0.4, center = Vec(0, -2, -1)), modelRotation = rot(y = 70), durationInSec = 130),
        CameraConfig("c", Cam.cameras(33, 33, 10, eccentricity = 0.9, offset = Vec(0, 0, 10)), modelRotation = rot(y = -30), durationInSec = 130),
      ))),
    ),
    GaiaImage(id = "sund6",
      desc = "stars as spheres with direction color coded. 8 to 23 kpc",
      fCreateModel = ImageFactory.sund6,
      hpOrder = Some(150),
      backColor = Color.black,
      video = Some("https://www.youtube.com/embed/j1GaECAYAi8"),
      textVal = Some(
        """
          |Movement of stars around the sun in a distance between 8 and 23 kpc.
          |""".stripMargin.trim
      ),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", 
          Cam.cameras(0, 6, 20, eccentricity = 0.1), 
          durationInSec = 130),
        CameraConfig("b", 
          Cam.cameras(44, 6, 30, eccentricity = 0.95, reverse = true, offset = Vec(-3, -3, 1)), 
          modelRotation = rot(y = 60), durationInSec = 130),
        CameraConfig("c", 
          Cam.cameras(10, 40, 15, eccentricity = 0.3, offset = Vec(-3, -3, 10)), 
          modelRotation = rot(y = -30), durationInSec = 130),
      ))),
    ),
    GaiaImage(id = "dens1",
      desc = "density of stars as shown by gaia",
      fCreateModel = ImageFactory.dens,
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
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 6, 10, eccentricity = 0.1, offset = Vec(0, 0, 0)), 130),
        CameraConfig("b", Cam.cameras(80, 60, 30, eccentricity = 0.1, offset = Vec(0, 0, 0)), 130),
        CameraConfig("c", Cam.cameras(10, -30, 20, eccentricity = 0.5, offset = Vec(0, 8, 0), reverse = true), 130),
      ))),
    ),
    GaiaImage(id = "gc1",
      desc = "galactic center 1.7",
      textVal = Some("stars around the galactic center up to a distance of 1.7 kpc"),
      fCreateModel = ImageFactory.gc1,
      backColor = Color.veryDarkBlue,
      video = Some(" https://youtu.be/6ORL4caNz9g "),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("near", Cam.cameras(0, 20, 4), 80),
        CameraConfig("far", Cam.cameras(0, -45, 6, reverse = true), 80),
      ))),
      credits = CreditConfig(references = Seq(
        "creation: entelijan",
        "http://entelijan.net",
        "music: Dee Yan-Key",
        "https://freemusicarchive.org/music/Dee_Yan-Key",
      )),
    ),
    GaiaImage(id = "gcd1",
      desc = "around the galactic center",
      textVal = Some("stars around the galactic center up a distance of 3 kpc"),
      fCreateModel = ImageFactory.gcd1,
      backColor = Color.black,
      video = Some("https://youtu.be/ZP0GFgninZc"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("far", Cam.cameras(0, -45, 6, eccentricity = 0.7), 60),
        CameraConfig("near", Cam.cameras(33, 95, 3, reverse = true, eccentricity = 0.85), 60),
      ))),
      credits = CreditConfig(references = Seq(
        "creation: entelijan",
        "http://entelijan.net",
        "music: Satellite by Bio Unit",
        "https://freemusicarchive.org/music/Bio_Unit/aerostat/satellite",
      )),
    ),
    GaiaImage(id = "gcd2",
      desc = "around the galactic center",
      textVal = Some("stars around the galactic center up a distance of 2 kpc and their direction"),
      fCreateModel = ImageFactory.gcd2,
      backColor = Color.white,
      video = Some("https://youtu.be/m6VKmQIKj04"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("far", Cam.cameras(22, -66, 3, eccentricity = 0.8, offset = Vec(0, 2, 1)), 60),
        CameraConfig("equator", Cam.cameras(99, 5, 4, eccentricity = 0.7, offset = Vec(0, 0, 0.5)), 60),
        CameraConfig("top", Cam.cameras(99, 5, 2, eccentricity = 0.4, offset = Vec(0, 0, 3)), 60),
      ))),
      credits = CreditConfig(references = Seq(
        "creation: entelijan",
        "http://entelijan.net",
        "music: 	Leafeaters by Podington Bear",
        "https://freemusicarchive.org/music/Podington_Bear",
      )),
    ),
    GaiaImage(id = "gc3",
      desc = "around the galactic center",
      textVal = Some("stars around the galactic center up a distance of 1.6 kpc"),
      fCreateModel = ImageFactory.gc3,
      backColor = Color.black,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(22, -10, 3, eccentricity = 0.2, offset = Vec(0, 0, 0)),
          durationInSec = 120,
          modelRotation = rot(y = 15, z = 22)),
        CameraConfig("b",
          Cam.cameras(44, 44, 4, eccentricity = 0.9, offset = Vec(1, 1, 3)),
          durationInSec = 120,
          modelRotation = rot(y = 66, z = 22)),
      ))),
    ),
    GaiaImage(id = "gcd4",
      desc = "around the galactic center",
      fCreateModel = ImageFactory.gcd4,
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(22, -66, 3, eccentricity = 0.8, offset = Vec(0, 2, 1)), 60),
      ))),
      credits = CreditConfig(references = Seq(
        "creation: entelijan",
        "http://entelijan.net",
      )),
    ),
  ))

  def startGaia(args: Array[String]): Unit = {
    def rest(args: Iterable[String]): List[String] = args.toList match {
      case Nil => List.empty[String]
      case _ :: rest => rest
    }

    def usage(message: Option[String]): Unit = {
      val drawingIds = actions
        .toSeq
        .sortBy(_._1)
        .map(_._2)
        .map(d => f"  ${d.id}%7s | ${d.desc}").mkString("\n")
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

    if (args.length >= 1) {
      val actionId = args(0)
      try {
        println(s"Run $actionId. Workdir ${workPath.toAbsolutePath}")
        actions.get(actionId).map(c => c.call(rest(args), workPath)).getOrElse(usage(Some(s"Illegal Action ID $actionId")))
      } catch {
        case e: IllegalArgumentException => usage(Some(e.getMessage))
      }
    } else
      usage(Some("You must define an Action-ID"))
  }

  private def createX3d(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = true

    def exec(gi: GaiaImage, wp: Path): Unit = {
      println(s"Creating gaia x3d model for ID ${gi.id}. ${gi.desc}")
      val outdir = workPath.resolve(gi.id).resolve("models")
      if Files.notExists(outdir) then Files.createDirectories(outdir)
      writeModelToFile(gi, outdir.resolve(s"${gi.id}.x3d"))
    }

    createSomething(args, "x3d model", workPath, filter, exec)
  }


  private def createX3dAnimation(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.videoConfig.map(vc => vc.isInstanceOf[VideoConfig.Cams]).getOrElse(false)

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach {
      case VideoConfig.Cams(camConfigs) => {
        println(s"Creating gaia x3d for ID ${gi.id}. ${gi.desc}")
        val modelsDirir = workPath.resolve(gi.id).resolve("models")
        if Files.notExists(modelsDirir) then Files.createDirectories(modelsDirir)
        val shapables = gi.fCreateModel(workPath, gi.backColor)
        gi.videoConfig.foreach {
          case VideoConfig.Call(_) =>
            throw IllegalStateException(
              "X3d animation cannot be created with VideoConfig.Call. You need VideoConfig.Cams")
          case VideoConfig.Cams(camConfs) => camConfs.foreach { camConfig =>
            val file = modelsDirir.resolve(s"${gi.id}_${camConfig.id}_animation.x3d")
            val xml = X3d.createCamAnimatedXml(shapables, camConfig.cams(500), gi.backColor, camConfig.durationInSec, camConfig.modelRotation)
            gaia.Util.writeString(file, xml)
            println(s"Created x3d animation for ${gi.id} at ${file.toAbsolutePath}")
          }
        }
      }
      case _ => throw IllegalStateException("You should never come here...")
    }

    createSomething(args, "x3d animated model", workPath, filter, exec)
  }

  private def createStill(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.videoConfig.map(vc => vc.isInstanceOf[VideoConfig.Cams]).getOrElse(false)

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach {
      case VideoConfig.Cams(camConfigs) => Cam.mkStillcameraConfig(gi, camConfigs, workPath)
      case _ => throw IllegalStateException("You should never come here...")
    }

    createSomething(args, "still images", workPath, filter, exec)
  }

  private def createCredits(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = true

    def exec(gi: GaiaImage, wp: Path): Unit = Cred.create(gi, wp)

    createSomething(args, "credits", workPath, filter, exec)
  }

  private def createCreditsTxt(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = true

    def exec(gi: GaiaImage, wp: Path): Unit = Cred.createTxt(gi, wp)

    createSomething(args, "text credits", workPath, filter, exec)
  }

  private def createVideo(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.videoConfig.isDefined

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach {
      case VideoConfig.Call(f) => f(gi, wp, false)
      case VideoConfig.Cams(camConfigs) =>
        Cam.mkVideoCameraConfig(gi, camConfigs, workPath, gi.videoQuality)
    }

    createSomething(args, "videos", workPath, filter, exec)
  }

  private def createPreviewVideo(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.videoConfig.isDefined

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach {
      case VideoConfig.Call(f) => f(gi, wp, true)
      case VideoConfig.Cams(camConfigs) =>
        Cam.mkVideoCameraConfig(gi, camConfigs, workPath, VideoQuality.defaultPreview)
    }

    createSomething(args, "videos", workPath, filter, exec)
  }

  private def createSomething(args: List[String], name: String, workPath: Path,
                              f: (gaiaImage: GaiaImage) => Boolean,
                              e: (gaiaImage: GaiaImage, wp: Path) => Unit): Unit = {
    val validImages = images.toList.filter { (_, i) => f(i) }.toMap
    val idsStr = validImages.values match {
      case l if l.isEmpty => "(None)"
      case l => l.map(i => i.id).mkString(",")
    }
    val info = "Valid IDs: " + idsStr
    if (args.size < 1) throw IllegalArgumentException(s"Define an ID for creating $name. $info")
    val id = args.head
    validImages.get(id) match {
      case None => throw IllegalArgumentException(s"Unknown ID $id for creating $name. $info")
      case Some(gaiaImage) =>
        println(s"Creating a $name for ID ${gaiaImage.id}. ${gaiaImage.desc}")
        e(gaiaImage, workPath)
        println(s"Created a $name for ID ${gaiaImage.id}. ${gaiaImage.desc}")
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
    if (env != null && env.nonEmpty) {
      val base = Path.of(env)
      workFromBase(base)
    } else {
      val home = Path.of(System.getProperty("user.home"))
      val base = home.resolve("work")
      workFromBase(base)
    }
  }

  def rot(x: Int = 0, y: Int = 0, z: Int = 0): Rotation1 = Rotation1(degToRad(x), degToRad(y), degToRad(z))

}



package gaia

import gaia.Gaia.workPath

import java.nio.file.Files.{createDirectories, notExists}
import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random
import entelijan.viz.{Viz, VizCreator, VizCreators, DefaultDirectories}
import scala.jdk.StreamConverters._
import java.time.format.DateTimeFormatter
import java.time.LocalDateTime


object Gaia {

  import Data.Star
  import ImageUtil._
  import X3d._
  import Vector._

  private lazy val workPath: Path = getCreateWorkPath

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

    def stillPreview = VideoQuality(VideoResolution.FullHD, antiAliasing = 3, frameRate = 60)
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
                        youTubeId: Seq[String] = Seq(),
                        hpOrder: Option[Int] = None,
                        textVal: Option[String] = None,
                        realCreatable: Boolean = true,
                        videoConfig: Option[VideoConfig] = None,
                        backColor: Color = Color.black,
                        videoQuality: VideoQuality = VideoQuality.default,
                        credits: CreditConfig  = CreditConfig(references = Seq(
                            "creation: entelijan",
                            "http://entelijan.net",
                          )),
                        seed: Long = 2348948509348L,
                        fDiagram: Option[(workPath: Path) => Viz.Dia[Viz.XY]] = None
                      ) extends Identifiable {
    def text: String = if (textVal.isDefined) textVal.get else desc

    def renderWithBrowser: Boolean = hpOrder.isDefined
  }

  val actions: Map[String, Action] = identifiableToMap(Seq(
    Action("hp", "create homepage files and snipplets", createHp(copyResources = false)),
    Action("x3d", "create a x3d model", createX3d),
    Action("x3da", "create an animated x3d model", createX3dAnimation),
    Action("vid", "create video snippets", createVideo),
    Action("vidp", "create preview video snippets", createPreviewVideo),
    Action("vidc", "create complet video from snippets", createCompleteVideo),
    Action("still", "create still images", createStill),
    Action("tryout", "Tryout something during development", Tryout.doit),
    Action("dia", "create diagram", createDiagram),
  ))

  val images: Map[String, GaiaImage] = identifiableToMap(Seq(
    GaiaImage("sunos1",
      desc = "around the sun 7 to 9 kpc",
      textVal = Some(
        """One shell around the sun between 7 and 9 kpc. 
          |The shell contains 2710 Stars which are visualized as spheres. 
          |The sun and the galactic center is displayed as cross hairs.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunos1,
      hpOrder = Some(20),
      youTubeId = Seq("jAuJPadoYvs"),
      backColor = Color.darkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("far", Cam.cameras(90, 20, 15), 130),
        CameraConfig("ecc", Cam.cameras(120, -30, 15, eccentricity = 0.8, offset = Vec(0, 0, 5)), 130),
        CameraConfig("aeq", Cam.cameras(0, 60, 18, eccentricity = 0.7, offset = Vec(3, 0, 3)), 130),
      ))),
    ),
    GaiaImage("sunos2", 
      desc = "around the sun 7 to 9 kpc",
      textVal = Some(
        """One shell around the sun between 7 and 9 kpc. The shell contains 2710 Stars which are visualized as spheres. 
          |The sun and the galactic center is displayed as cross hairs.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunos2,
      hpOrder = Some(10),
      backColor = Color.darkBlue,
      youTubeId = Seq("cEVH0IhlJ4Y"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("aqu", Cam.cameras(0, 15, 15, eccentricity = 0.7), 100, rot(y = 60)),
        CameraConfig("pol", Cam.cameras(100, 80, 22, eccentricity = 0.5), 100)
      ))),
    ),
    GaiaImage("sunms1", 
      desc = "around the sun 5 8 11 kpc",
      textVal = Some(
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 1700, 2000 and 1000 Stars from the inner to the outer shell which are visualized as spheres. 
          |The sun and the galactic center is displayed as cross hairs.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunms1,
      hpOrder = Some(40),
      youTubeId = Seq("irbUh9Y_Ifg"),
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
    ),
    GaiaImage("sunms2", 
      desc = "around the sun 5 8 11 kpc",
      textVal = Some(
        """Three shells around the sun with distances 5, 8 and 11 kpc. 
          |The shells contains 4000, 8000 and 14000 Stars from the inner to the outer shell which are visualized as points. 
          |The sun and the galactic center is displayed as cross hairs.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunms2,
      hpOrder = Some(30),
      backColor = Color.darkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("simple",
          Cam.cameras(0, 15, 25, eccentricity = 0.0, offset = Vec(0, 0, 0)),
          durationInSec = 60),
      ))),
      youTubeId = Seq("JelflHQSamo"),
    ),
    GaiaImage("sunnear1", 
      desc = "around the sun 2 kpc",
      textVal = Some(
        """Stars around the sun with a maximum distance of 2 kpc. Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunnear1,
      backColor = Color.black,
      youTubeId = Seq("lp-Y_jpYmnw"),
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
    GaiaImage("sunnear2", 
      desc = "around the sun 5 kpc",
      textVal = Some(
        """Stars around the sun with a maximum distance of 5 kpc. Some stars are filtered out to make the image clearer. 
          |Near the center more stars are filtered out than close to the edge to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunnear2,
      backColor = Color.black,
      youTubeId = Seq("aH2YRU485vk"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 7, eccentricity = 0.7, offset = Vec(1, 0, 0)),
          durationInSec = 60),
        CameraConfig("b",
          Cam.cameras(120, 50, 7, eccentricity = 0.9, offset = Vec(5, 0, 3), reverse = true),
          durationInSec = 60),
      ))),
    ),
    GaiaImage("sunnear3", 
      desc = "around the sun 8 kpc",
      textVal = Some(
        """Stars around the sun with a maximum distance of 8 kpc.
          |Some stars are filtered out to make the image clearer.
          |Near the center more stars are filtered out than close to the edge
          |to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sunnear3,
      youTubeId = Seq("LbW1O-GUPS8"),
      backColor = Color.black,
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
    GaiaImage("sun16", 
      desc="around the sun 16 kpc",
      textVal = Some(
        """Stars around the sun with a maximum distance of 16 kpc. Some stars are filtered out to make the image clearer. 
          |Near the sun more stars are filtered out than at to the edge to avoid bulges around the sun.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sun16,
      youTubeId = Seq("thGXoBR6ewA"),
      backColor = Color.black,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 7, eccentricity = 0.7, offset = Vec(1, 0, 0)),
          durationInSec = 60),
        CameraConfig("b",
          Cam.cameras(90, -35, 8, eccentricity = 0.9, offset = Vec(5, 1, 0), reverse = true),
          durationInSec = 60),
      ))),
    ),
    GaiaImage("sund27", 
      desc = "around the sun 27 pc",
      textVal = Some(
        """Stars around the sun up to a distance of 27 pc. 
          |Proper motion of the stars is symbolized by the length of cylinders.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sund27,
      backColor = Color.veryDarkGreen,
      hpOrder = Some(145),
      youTubeId = Seq("JuK80k5m4vU"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(20, 15, 0.01, eccentricity = 0.9, offset = Vec(0, 0, 0)),
          durationInSec = 60),
      ))),
    ),
    GaiaImage(id = "sund1",
      desc = "around the sun 40 pc",
      textVal = Some(
        """Proper motion of stars around the sun up to a distance of 40 pc.
          |""".stripMargin),
      fCreateModel = ImageFactory.sund1,
      hpOrder = Some(106),
      youTubeId = Seq("AZaBZWo0uwQ"),
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
    GaiaImage("sund2", 
      desc = "around the sun 40 pc radial",
      textVal = Some(
        """Proper motion of stars around the sun up to a distance of 40 pc. 
          |Distance of all stars is set to 40 pc. The direction of all motion is set to radial from the sun. 
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sund2,
      hpOrder = Some(105),
      youTubeId = Seq("Fee7OpM1Kno"),
      backColor = Color.black,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 80, 0.1), 60),
        CameraConfig("b", Cam.cameras(0, 20, 0.2, eccentricity = 0.99), 60),
      ))),
    ),
    GaiaImage("sund3", 
      desc = "around the sun 45 pc",
      textVal = Some(
        """Stars and their proper motion around the sun up to a distance of 45 pc
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sund3,
      hpOrder = Some(110),
      youTubeId = Seq("hUqVxwHVTZg"),
      backColor = Color.veryDarkGreen,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 40, 0.08), 60),
        CameraConfig("b", Cam.cameras(33, 60, 0.12, eccentricity = 0.8), modelRotation = rot(y = 45), durationInSec = 60),
        CameraConfig("c", Cam.cameras(99, 20, 0.2, eccentricity = 0.999), modelRotation = rot(y = -45), durationInSec = 60),
      ))),
    ),
    GaiaImage("sund4", 
      desc = "around the sun 8 kpc",
      textVal = Some(
        """Stars and their proper motion around the sun at a distance of 8 kpc.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sund4,
      hpOrder = Some(120),
      youTubeId = Seq("bZ0KkVM-Kwc"),
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 60, 20, eccentricity = 0.9), 120),
        CameraConfig("b", Cam.cameras(90, -40, 20, eccentricity = 0.6, center = Vec(0, -5, -1)), modelRotation = rot(z = 20), durationInSec = 120),
        CameraConfig("c", Cam.cameras(0, 5, 15, offset = Vec(0, 0, -10)), modelRotation = rot(y = 30), durationInSec = 120),
      ))),
    ),
    GaiaImage(id = "sund5",
      desc = "around the sun 6 8 10 kpc",
      textVal = Some(
        """Proper motion of stars around the sun in 3 shells (6, 8 and 10 kpc). 
          |Cross hairs indicate the sun and the center of the galaxy.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sund5,
      hpOrder = Some(140),
      youTubeId = Seq("NWRHYBLjFv0"),
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 6, 20, eccentricity = 0.1), 130),
        CameraConfig("b", Cam.cameras(90, -40, 15, eccentricity = 0.4, center = Vec(0, -2, -1)), modelRotation = rot(y = 70), durationInSec = 130),
        CameraConfig("c", Cam.cameras(33, 33, 10, eccentricity = 0.9, offset = Vec(0, 0, 10)), modelRotation = rot(y = -30), durationInSec = 130),
      ))),
    ),
    GaiaImage(id = "sund6",
      desc = "around the sun between 8 and 23 kpc",
      textVal = Some(
        """Proper Motion of stars around the sun in a distance between 8 and 23 kpc.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.sund6,
      hpOrder = Some(150),
      backColor = Color.black,
      youTubeId = Seq("CxndKkrbk-4"),
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
      desc = "density of stars as gaia sees it",
      textVal = Some(
        """Density of stars as seen by gaia in a cube around the center of galaxy. 
          |The side of that cube is 16 kpc. The number of stars in different 
          |sectors is shown by spheres with different size.
          |""".stripMargin.trim
      ),
      fCreateModel = ImageFactory.dens,
      hpOrder = Some(170),
      backColor = Color.veryDarkBlue,
      youTubeId = Seq("FMIKp63XT1U", "SjuCFmUmQQk"),
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, 6, 10, eccentricity = 0.1, offset = Vec(0, 0, 0)), 130),
        CameraConfig("b", Cam.cameras(80, 60, 30, eccentricity = 0.1, offset = Vec(0, 0, 0)), 130),
        CameraConfig("c", Cam.cameras(10, -30, 20, eccentricity = 0.5, offset = Vec(0, 8, 0), reverse = true), 130),
      ))),
    ),
    GaiaImage(id = "gc1",
      desc = "around the galactic center 1.7 kpc",
      textVal = Some(
        """Stars around the galactic center up to a distance of 1.7 kpc. 
          |The galactic plane is symbolized by circles.
          |""".stripMargin),
      hpOrder = Some(200),
      fCreateModel = ImageFactory.gc1,
      backColor = Color.veryDarkBlue,
      youTubeId = Seq("6ORL4caNz9g"),
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
      desc = "around the galactic center 3 kpc",
      textVal = Some(
        """Stars around the galactic center up to a distance of 3 kpc. 
          |The proper motion of the stars is indicated by little ‘tails’
          |""".stripMargin),
      hpOrder = Some(210),
      fCreateModel = ImageFactory.gcd1,
      backColor = Color.black,
      youTubeId = Seq("ZP0GFgninZc"),
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
      desc = "around the galactic center 2 kpc with movements",
      textVal = Some(
        """Stars around the galactic center with direction and velocity of their proper motion up to a distance of 2 kpc.
          |""".stripMargin),
      hpOrder = Some(220),
      fCreateModel = ImageFactory.gcd2,
      backColor = Color.white,
      youTubeId = Seq("m6VKmQIKj04"),
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
      desc = "around the galactic center 1.6 kpc",
      textVal = Some(
        """Stars around the galactic center up to a distance of 1.6 kpc. 
          |The galactic plane is symbolized by circles.
          |""".stripMargin),
      fCreateModel = ImageFactory.gc3,
      hpOrder = Some(230),
      backColor = Color.black,
      youTubeId = Seq("AE6muFf1nvA"),
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
      desc = "horizontal slices around the galactic center",
      textVal = Some(
        """Proper motions of stars in slices 1 and 3 kpc away from the galactic plane.
          |""".stripMargin),
      hpOrder = Some(240),
      youTubeId = Seq("jXdOY-bJ5Ns"),
      fCreateModel = ImageFactory.gcd4,
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, -20, 3, eccentricity = 0.5, offset = Vec(0, 0, 0), center = Vec(0, 0, 5)), 60),
        CameraConfig("b", Cam.cameras(0, -10, 5, eccentricity = 0.2, offset = Vec(0, 0, 0)), durationInSec = 60, modelRotation = rot(y = 80)),
        CameraConfig("c", Cam.cameras(0, 30, 10, eccentricity = 0.9, offset = Vec(2, 0, 2)), durationInSec = 60, modelRotation = rot(y = -70)),
      ))),
    ),
    GaiaImage(id = "gcd5",
      desc = "around the galactic center 2 kpc",
      textVal = Some(
        """Proper motion of stars around the galactic center up to a distance of 2 kpc away from the center. 
          |Distance from the galactic plane (z) of all stars is set to zero.
          |""".stripMargin),
      hpOrder = Some(250),
      fCreateModel = ImageFactory.gcd5,
      youTubeId = Seq("Ktimgtm991A"),
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a", Cam.cameras(0, -20, 3, eccentricity = 0.5, offset = Vec(0, 0, 0), center = Vec(0, 0, 0)), durationInSec = 130, modelRotation = rot(y = 120)),
        CameraConfig("b", Cam.cameras(0, -30, 6, eccentricity = 0.9, offset = Vec(-2, 0, 2), center = Vec(0, 0, 0)), durationInSec = 130),
      ))),
    ),
    GaiaImage(id = "gcs1",
      desc = "around the galactic center normalized by sector",
      textVal = Some(
        """Stars around the galactic center within a cylinder of radius 5 kpc and height 4 kpc. 
          |The amount of stars in ten equally wide sectors is normalized in the way that every sector 
          |has about the same amount of stars as the sector with the least amount of stars
          |""".stripMargin),
      hpOrder = Some(260),
      fCreateModel = ImageFactory.gcs1,
      youTubeId = Seq("Xmheem6PED0"),
      backColor = Color.veryDarkBlue,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(0, -20, 6, eccentricity = 0.2, offset = Vec(0, 0, 0), center = Vec(0, 0, 0)),
          durationInSec = 130, modelRotation = rot(y = 0)),
        CameraConfig("b",
          Cam.cameras(0, 50, 12, eccentricity = 0.9, offset = Vec(0, 0, 0), center = Vec(0, 0, 0)),
          durationInSec = 130, modelRotation = rot(y = 80)),
        CameraConfig("c",
          Cam.cameras(0, 30, 15, eccentricity = 0.8, offset = Vec(5, 0, 3), center = Vec(0, 0, 0)),
          durationInSec = 130, modelRotation = rot(x = 85)),
      ))),
      seed = 304334L,
    ),
    GaiaImage(id = "gcs2",
      desc = "around the galactic center normalized by sector",
      textVal = Some(
        """Stars around the galactic center within a cylinder of radius 5 kpc and height from 2 to 3 kpc above the galactic plane. 
          |The amount of stars in ten equally wide sectors is normalized in the way that every sector 
          |has about the same amount of stars as the sector with the least amount of stars.
          |""".stripMargin),
      hpOrder = Some(270),
      youTubeId = Seq("Xmheem6PED0"),
      fCreateModel = ImageFactory.gcs2,
      backColor = Color.black,
      videoConfig = Some(VideoConfig.Cams(Seq(
        CameraConfig("a",
          Cam.cameras(0, 30, 6, eccentricity = 0.2, offset = Vec(0, 0, 0), center = Vec(0, 0, 2)),
          durationInSec = 130, modelRotation = rot(y = 0)),
        CameraConfig("b",
          Cam.cameras(-60, 50, 15, eccentricity = 0.7, offset = Vec(0, 0, 0), center = Vec(0, 0, 0)),
          durationInSec = 130, modelRotation = rot(y = 88)),
      ))),
      credits = CreditConfig(references = Seq(
        "creation: entelijan",
        "http://entelijan.net",
      )),
      fDiagram = Some(DiagramFactory.gcs2)
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

  def createDiagram(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.fDiagram.isDefined

    def exec(gi: GaiaImage, wp: Path): Unit = {
      val outDir = outPath(wp).resolve(Path.of(gi.id, "diagram"))
      if Files.notExists(outDir) then Files.createDirectories(outDir)

      Util.runWithTmpdir() { tmpDir =>
        implicit val creator: VizCreator[Viz.XY] =
          VizCreators.gnuplot(
            scriptDir = tmpDir.toFile,
            imageDir = outDir.toFile,
            clazz = classOf[Viz.XY])
        val dia = gi.fDiagram.get(wp)
        Viz.createDiagram(dia)
      }
      println(s"wrote diagram to $outDir")
    }

    createSomething(args, "diagram", workPath, filter, exec)
  }

  def outPath(workPath: Path): Path = {
    val outPath = workPath.resolve("out")
    if Files.notExists(outPath) then Files.createDirectories(outPath)
    outPath
  }

  private def createHp(copyResources: Boolean)(args: List[String], workPath: Path): Unit = {
    Hp1.createHp(workPath, gaiaImages = images.values.toSeq, copyResources)
  }

  def createX3d(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = true

    def exec(gaiaImage: GaiaImage, workPath: Path): Unit = {
      val shapables = gaiaImage.fCreateModel(workPath, gaiaImage.backColor)
      val idir = Util.fileDirInOutDir(workPath, gaiaImage.id)
      val modelsPath = Util.fileDirFromDir(idir, "models")
      val fileName = s"${gaiaImage.id}.x3d"
      val xml = X3d.createXml(shapables, fileName, gaiaImage.backColor)
      val file = modelsPath.resolve(fileName)
      Util.writeString(file, xml)
      println(s"Created image for ${gaiaImage.id} at ${file.toAbsolutePath}")
    }

    createSomething(args, "x3d model", workPath, filter, exec)
  }


  def createX3dAnimation(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.videoConfig.map(vc => vc.isInstanceOf[VideoConfig.Cams]).getOrElse(false)

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach {
      case VideoConfig.Cams(camConfigs) => {
        println(s"Creating gaia x3d for ID ${gi.id}. ${gi.desc}")
        val modelsDir = outPath(workPath).resolve(gi.id).resolve("animated-models")
        if Files.notExists(modelsDir) then Files.createDirectories(modelsDir)
        val shapables = gi.fCreateModel(workPath, gi.backColor)
        gi.videoConfig.foreach {
          case VideoConfig.Call(_) =>
            throw IllegalStateException(
              "X3d animation cannot be created with VideoConfig.Call. You need VideoConfig.Cams")
          case VideoConfig.Cams(camConfs) => camConfs.foreach { camConfig =>
            val file = modelsDir.resolve(s"${gi.id}_${camConfig.id}_animation.x3d")
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

  def createStill(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = gi.videoConfig.map(vc => vc.isInstanceOf[VideoConfig.Cams]).getOrElse(false)

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach {
      case VideoConfig.Cams(camConfigs) => Cam.mkStillcameraConfig(gi, camConfigs, workPath)
      case _ => throw IllegalStateException("You should never come here...")
    }

    createSomething(args, "still images", workPath, filter, exec)
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

  private def createCompleteVideo(args: List[String], workPath: Path): Unit = {
    def hasVideoSnippets(gi: GaiaImage): Boolean = {
      val idir = Util.fileDirInOutDir(workPath, gi.id)
      val vdir = idir.resolve("videos")
      if Files.notExists(vdir) then false
      else !Files.list(vdir)
        .toScala(Seq)
        .filter(p => p.getFileName.toString.endsWith(".mp4"))
        .isEmpty
    }

    def timestamp: String = {
      DateTimeFormatter.ofPattern("yyyyMMddHHmmss").format(LocalDateTime.now)
    }

    def filter(gi: GaiaImage): Boolean = hasVideoSnippets(gi)

    def exec(gi: GaiaImage, wp: Path): Unit = {
      val idir = Util.fileDirInOutDir(workPath, gi.id)
      val outDir = Util.fileDirFromDir(idir, "videos-complete")
      val outFile = outDir.resolve(s"gaia-visual-${gi.id}-$timestamp.mp4")
      Cam.mkCompleteVideo(gi, outFile, workPath)
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
        Random.setSeed(gaiaImage.seed)
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



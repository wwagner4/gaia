package gaia


import java.nio.file.{Files, Path}
import scala.io._
import scala.util.Random


object Main {

  import Data.Star
  import ImageUtil._
  import X3d._
  import Vector._

  lazy val workPath = getCreateWorkPath

  trait Identifiable {
    def id: String
  }

  case class Action(
                     id: String,
                     desc: String,
                     call: (args: List[String], workPath: Path) => Unit
                   ) extends Identifiable


  type FuncVideoConfig = (gaiaImage: GaiaImage, workDir: Path) => Unit

  case class VideoConfig(
                          funcVideoConfig: FuncVideoConfig,
                          funcPreviewVideoConfig: Option[FuncVideoConfig] = None,
                        )

  case class GaiaImage(
                        id: String,
                        desc: String,
                        fCreateModel: (workPath: Path, backColor: Color) => Seq[Shapable],
                        video: Option[String] = None,
                        hpOrder: Option[Int] = None,
                        textVal: Option[String] = None,
                        realCreatable: Boolean = true,
                        videoConfig: Seq[VideoConfig] = Seq.empty[VideoConfig],
                        backColor: Color = Color.black,
                      ) extends Identifiable {
    def text: String = if (textVal.isDefined) textVal.get else desc

    def renderWithBrowser: Boolean = hpOrder.isDefined
  }

  val actions = identifiableToMap(Seq(
    Action("hp", "create homepage files and snipplets", gaia.Hp.createHp),
    Action("x3d", "create x3d files for an image", createX3d),
    Action("vid", "create video sniplets from an existing x3d model", createVideo(_, _)),
    Action("vidprev", "create preview video sniplets from an existing x3d model", createPreviewVideo(_, _)),
    Action("tryout", "Tryout something during development by callin doIt()", Tryout.doit),
  ))

  val images: Map[String, GaiaImage] = identifiableToMap(Seq(
    GaiaImage("sunos1", "One shell around the sun. Stars as spheres",
      ImageFactory.sunos1,
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
      videoConfig = Seq(VideoConfig(Automove.sunos2)),
    ),
    GaiaImage("sunms1", "Multiple shells around the sun. Stars as spheres",
      ImageFactory.sunms1,
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
      video = Some("https://www.youtube.com/embed/JelflHQSamo"),
      videoConfig = Seq(VideoConfig(Automove.sunms2))
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
      videoConfig = Seq(VideoConfig(Automove.sunnear1)),
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
      )
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
      )
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
      )
    ),
    GaiaImage("sund27", "direction of stars within a distance of 27 pc to sun",
      ImageFactory.sund27,
      hpOrder = Some(90),
      backColor = Color.veryDarkGreen,
      video = Some("https://www.youtube.com/embed/JuK80k5m4vU"),
      videoConfig = Seq(VideoConfig(Automove.sund27)),
    ),
    GaiaImage("sund1", "direction and velocety of stars to a distace of 40 pc",
      ImageFactory.sund1,
      hpOrder = Some(100),
      backColor = Color.veryDarkGreen,
    ),
    GaiaImage("sund2", "direction and velocety of stars in shell with distance 40 pc",
      ImageFactory.sund2,
      hpOrder = Some(105),
      backColor = Color.black,
    ),
    GaiaImage("sund3", "direction and velocety of stars of 45 pc distance",
      ImageFactory.sund3,
      hpOrder = Some(110),
      video = Some("https://www.youtube.com/embed/hUqVxwHVTZg"),
      backColor = Color.veryDarkGreen,
      videoConfig = Seq(VideoConfig(Automove.sund3)),
    ),
    GaiaImage("sund4", "direction and velocety of stars  8 kpc from the sun",
      ImageFactory.sund4,
      hpOrder = Some(120),
      video = Some("https://www.youtube.com/embed/bZ0KkVM-Kwc"),
      backColor = Color.veryDarkBlue,
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
      videoConfig = Seq(VideoConfig(Automove.sund5)),
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
          |Crosshairs indicate the sun and the center of the galaxy
          |""".stripMargin.trim
      ),
      videoConfig = Seq(VideoConfig(Automove.sund6)),
    ),
    GaiaImage(id = "gc1",
      desc = "around the galactic center",
      fCreateModel = ImageFactory.gc1,
      backColor = Color.veryDarkBlue,
      videoConfig = Seq(VideoConfig(Cam.gc1, Some(Cam.gc1Prev))),
    ),
    GaiaImage(id = "gcd1",
      desc = "around the galactic center",
      fCreateModel = ImageFactory.gcd1,
      backColor = Color.veryDarkBlue,
    ),
    GaiaImage(id = "gcd2",
      desc = "around the galactic center",
      fCreateModel = ImageFactory.gcd2,
      backColor = Color.veryDarkBlue,
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
      videoConfig = Seq(VideoConfig(Automove.dens1)),
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
        writeModelToFile(gaiaImage, workPath)
    }
  }

  private def createVideoBase(args: List[String], workPath: Path, f: (gaiaImage: GaiaImage) => Boolean, e: (gaiaImage: GaiaImage, wp: Path) => Unit): Unit = {
    val validImages = images.toList.filter { (k, i) => f(i) }.toMap
    val idsStr = validImages.values match {
      case l if l.isEmpty => "(None)"
      case l => l.map(i => i.id).mkString(",")
    }
    val info = "Valid IDs: " + idsStr
    if (args.size < 1) throw IllegalArgumentException(s"Define an ID for creating videos. $info")
    val id = args(0)
    validImages.get(id) match {
      case None => throw IllegalArgumentException(s"Unknown ID $id for creating videos. $info")
      case Some(gaiaImage) =>
        println(s"Creating a video for ID ${gaiaImage.id}. ${gaiaImage.desc}")
        e(gaiaImage, workPath)
    }
  }

  private def createVideo(args: List[String], workPath: Path): Unit = {
    def filter(gi: GaiaImage): Boolean = !gi.videoConfig.isEmpty

    def exec(gi: GaiaImage, wp: Path): Unit = gi.videoConfig.foreach(_.funcVideoConfig(gi, wp))

    createVideoBase(args, workPath, filter, exec)
  }

  private def createPreviewVideo(args: List[String], workPath: Path): Unit = {
    def filter(gaiaImage: GaiaImage): Boolean = !gaiaImage.videoConfig.isEmpty && gaiaImage.videoConfig.exists(c => c.funcPreviewVideoConfig.isDefined)

    def exec(gaiaImage: GaiaImage, wp: Path): Unit = gaiaImage.videoConfig.foreach(vc => vc.funcPreviewVideoConfig.foreach(f => f(gaiaImage, wp)))

    createVideoBase(args, workPath, filter, exec)
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



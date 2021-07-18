package gaia

import entelijan.viz.Viz.XY
import entelijan.viz.{DefaultDirectories, Viz, VizCreator, VizCreators}
import entelijan.vizb.LineChartBuilder
import gaia.Gaia.outPath
import org.apache.commons.lang3.math.Fraction
import upickle.default.{macroRW, read, write, ReadWriter => RW}

import java.io.{BufferedReader, File, InputStream, InputStreamReader}
import java.net.URL
import java.nio.file.{Files, Path}
import java.util.UUID
import java.util.concurrent.{CompletableFuture, ExecutorService, Executors}
import java.util.function.Consumer
import java.util.zip.GZIPInputStream
import scala.annotation.tailrec
import scala.jdk.StreamConverters._
import scala.language.{implicitConversions, postfixOps}
import scala.util.Random
import scala.collection.parallel.CollectionConverters._


//noinspection ScalaUnusedSymbol
object Tryout {

  import Cam._
  import Data._
  import DataUtil._
  import Gaia.{GaiaImage, VideoConfig, workPath}
  import Hp1._
  import ImageUtil._
  import Vector._
  import X3d._


  def doit(args: List[String], workPath: Path): Unit = {
    Development.dens(workPath)
  }

  object Development {

    def dens(workPath: Path):Unit = {
      ???
    }

  }

  object Useful {

    case class DensConfig(
                           id: String,
                           cubeSplit: CubeSplit,
                           minCountPerSector: Int,
                           usageProbability: Double
                         )


    /**
     * Create and write a new dens file
     *
     * @param workPath : Wor path or the current installation
     */
    def densWrite(workPath: Path): Unit = {

      val configs = Seq(
        DensConfig("r100", CubeSplit.rough, 100, 0.1),
        DensConfig("r050", CubeSplit.rough, 50, 0.1),
        DensConfig("r020", CubeSplit.rough, 20, 0.1),
        DensConfig("r010", CubeSplit.rough, 10, 0.1),
        DensConfig("r005", CubeSplit.rough, 5, 0.1),
        DensConfig("m100", CubeSplit.medium, 100, 0.1),
        DensConfig("m050", CubeSplit.medium, 50, 0.1),
        DensConfig("m020", CubeSplit.medium, 20, 0.1),
        DensConfig("m010", CubeSplit.medium, 10, 0.1),
        DensConfig("m005", CubeSplit.medium, 5, 0.1),
      ).par

      configs.foreach { config =>
        println(s"Running dens $config")


        val stars = StarCollections.basicStars(workPath)

        val starsFiltered = stars.map(toStarPosDirGalactic)
          .filter(s => Random.nextDouble() <= 0.1 && s.pos.length < config.cubeSplit.cubeSize)

        val ic: (Vec, Cube) => Boolean = inCube(config.cubeSplit.cubeSize, config.cubeSplit.cubeCount)
        val counts: Seq[(Cube, Int)] = (for (c <- cubeIterator(config.cubeSplit.cubeCount)) yield {
          val sc = starsFiltered.map { s => if (ic(s.pos, c)) 1 else 0 }
          (c, sc.sum)
        }).sortBy((_, d) => -d)

        val probs: Seq[(Cube, Double)] = counts
          .map((c, cnt) => (c, math.min(1.0, config.minCountPerSector.toDouble / cnt)))
          .filter((_, prob) => prob < 1.0)

        println(s"size ${probs.size}")
        probs.foreach(println(_))
        val outDir = Util.fileDirInOutDir(workPath, "tryout-dens")
        val probFile = outDir.resolve(s"dens-${config.id}.pkl")

        Util.writeString(probFile, write(probs))
        println(s"Wrote probabillities to $probFile")
      }
    }

    def createAllX3d(workPath: Path): Unit = {
      Gaia.images.foreach { gi =>
        val id = gi._2.id
        println(s"Creating x3d for $id")
        Gaia.createX3d(List(id), workPath)
        Gaia.createX3dAnimation(List(id), workPath)
      }
    }

    def credits(workDir: Path): Unit = {

      def cred(gaiaImage: GaiaImage): String = {
        val name = s"${gaiaImage.id} ${gaiaImage.desc}"
        val desc = gaiaImage.text
        "--------------------------------------------\n" +
          name + "\n\n" + desc

      }

      val ids = Seq("sund6")
      imagesSorted
        .filter(gi => ids.contains(gi.id))
        .foreach(x => println(cred(x)))

    }

    def overlays(wp: Path): Unit = {
      imagesSorted.foreach { gaiaImage =>
        val outputImage = Util.fileDirInOutDir(wp, "tryout-htmlpng").resolve(s"${gaiaImage.id}.png")
        val logoFile = Path.of("src", "main", "html1", "res", "video-400.png")
        Hp1.logoImage(gaiaImage, logoFile, outputImage, wp)
      }
    }

    private def imagesSorted: Seq[GaiaImage] = {
      Gaia.images
        .values
        .toSeq
        .sortBy(_.id)
    }

    private def videoFiles(gaiaImage: GaiaImage, workPath: Path): Seq[Path] = {
      val idir = Util.fileDirInOutDir(workPath, gaiaImage.id)
      val vdir = Util.fileDirFromDir(idir, "videos")
      Files.list(vdir).toScala(Seq)
    }

    private def videoFilesComplete(gaiaImage: GaiaImage, workPath: Path): Seq[Path] = {
      val idir = Util.fileDirInOutDir(workPath, gaiaImage.id)
      val vdir = Util.fileDirFromDir(idir, "videos-complete")
      Files.list(vdir).toScala(Seq)
    }

    def infoCsv(workPath: Path): Unit = {

      def infoDescr(gaiaImage: GaiaImage): String = {
        val id = gaiaImage.id
        val desc = gaiaImage.desc
        val text = gaiaImage.textVal.map(tv => tv.replace("\n", " ")).getOrElse("")
        f"$id;$desc;$text"
      }

      def infoVideoSubdirs(gaiaImage: GaiaImage): String = {
        val id = gaiaImage.id
        val dubDirs = videoFiles(gaiaImage, workPath)
          .filter(p => Files.isDirectory(p))
          .map(p => p.getFileName)
          .mkString(",")
        f"$id;$dubDirs"
      }

      def infoSnippets(gaiaImage: GaiaImage): String = {

        def snipletInfos(files: Seq[Path]): String = {
          files.map(Hp1.vidInfo)
            .map(vi => ((vi.width, vi.height), vi))
            .groupBy(_._1)
            .toSeq
            .sortBy(-_._1._2)
            .map(t => (t._1, t._2.size))
            .map { (k, n) => s"[${k._1}, ${k._2}] $n" }
            .mkString(" ")
        }


        val id = gaiaImage.id
        val files = videoFiles(gaiaImage, workPath)
          .filter(p => !Files.isDirectory(p) && p.getFileName.toString.endsWith("mp4"))

        val results = if files.isEmpty then "" else snipletInfos(files)

        // f"$id;$results"
        f"$results"
      }

      def infoCompletes(gaiaImage: GaiaImage): String = {

        def completeInfos(files: Seq[Path]): String = {
          files.map(Hp1.vidInfo)
            .map(vi => ((vi.width, vi.height), vi))
            .groupBy(_._1)
            .toSeq
            .sortBy(-_._1._2)
            .map(t => (t._1, t._2.size))
            .map { (k, n) => s"[${k._1}, ${k._2}] $n" }
            .mkString(" ")
        }


        val files = videoFilesComplete(gaiaImage, workPath)
          .filter(p => !Files.isDirectory(p) && p.getFileName.toString.endsWith("mp4"))

        val results = if files.isEmpty then "" else completeInfos(files)

        //f"$id;$results"
        f"$results"
      }

      imagesSorted
        //.map(infoDescr)
        //.map(infoVideoSubdirs)
        .map(infoSnippets)
        //.map(infoCompletes)
        .foreach(println)
    }

    def videoInfo(workPath: Path): Unit = {
      case class VI(
                     gaiaImage: GaiaImage,
                     videoFiles: Seq[VidInfo],
                   )
      val infos = imagesSorted
        .map { gi =>
          val imgDir = Util.fileDirInOutDir(workPath, gi.id)
          val vidDir = imgDir.resolve("videos")
          val vis = if Files.exists(vidDir) then {
            Files.list(vidDir).toScala(Seq).filter(_.getFileName.toString.endsWith("mp4")).map(p => Hp1.vidInfo(p))
          } else Seq()
          VI(gi, vis)
        }
      infos.foreach { vi =>
        println(f"${vi.gaiaImage.id}")
        vi.videoFiles.sortBy(i => i.height).foreach(i => println(i))
      }
    }


    def cmds(): Unit = {
      imagesSorted
        .filter(_.videoConfig.exists(c => c.isInstanceOf[VideoConfig.Cams]))
        .map(_.id)
        .sorted
        .foreach { id =>
          println(s""";run still $id""")
        }
    }

    def overviewGaiaImages(workDir: Path): Unit = {

      def emptyImg =
        """<td></td>
          |""".stripMargin

      def validImg(p: Path) = {
        val workBaseEnv = System.getenv("GAIA_WORK_BASE")
        val workBase = if workBaseEnv == null then Path.of(System.getProperty("user.home")).resolve("work").toString else workBaseEnv

        val ps = p.toString.replace("/home/ugaia/work", workBase)
        s"""<td><img src="$ps" width="400"></td>
           |""".stripMargin
      }

      def tds(gaiaImage: GaiaImage): String = {
        val stills = Util.stillImageGroups(gaiaImage, workDir)
        if stills.isEmpty then Seq(emptyImg, emptyImg, emptyImg).mkString("\n")
        else Random.shuffle(stills).take(3).map(sig => validImg(sig.preview)).mkString("\n")
      }

      def tr(gaiaImage: GaiaImage): String = {
        s"""
           |<tr>
           |    <td>${gaiaImage.hpOrder.map(o => o.toString).getOrElse("-")}</td>
           |    <td>${gaiaImage.id} ${gaiaImage.desc}</td>
           |    <td>${gaiaImage.youTubeId.mkString(",")}</td>
           |${tds(gaiaImage)}
           |</tr>
           |""".stripMargin
      }

      def hpOrderedRevers(gi: GaiaImage): Int = {
        -gi.hpOrder.getOrElse(0)
      }

      def csvRow(gaiaImage: GaiaImage): Seq[String] = {
        Seq(
          gaiaImage.id,
          gaiaImage.desc,
          gaiaImage.youTubeId.mkString(","),
          gaiaImage.hpOrder.getOrElse(0).toString,
        )
      }

      def csvHead = Seq("id", "desc", "youtube", "hporder")


      val rows = imagesSorted
        .sortBy(gi => hpOrderedRevers(gi))
        .map(tr)
        .mkString("\n")

      val csvRows = imagesSorted
        .sortBy(gi => hpOrderedRevers(gi))
        .map(gi => csvRow(gi))

      val allRows = Seq(csvHead) ++ csvRows

      val csv = allRows
        .map(r => r.mkString("\t"))
        .mkString("\n")


      val html =
        s"""<html>
           |<body style="background-color: #78a4b5; font-family: sans-serif; font-size: 50px">
           |<h1>overview gaia images</h1>
           |<table  style="background-color: cadetblue; font-family: sans-serif; font-size: 20px">
           |    $rows
           |</table>
           |</body>
           |</html>
           |""".stripMargin

      val outDir = Util.fileDirInOutDir(workDir, "overview-images-html")
      val htmlFile = outDir.resolve("overview-gaia-images.html")
      val csvFile = outDir.resolve("overview-gaia-images.csv")
      Util.writeString(htmlFile, html)
      Util.writeString(csvFile, csv)
      println(s"Wrote overview to $htmlFile")
      println(s"Wrote overview to $csvFile")
    }

  }

  object Obsolete {

    def densRead(workPath: Path): Unit = {
      def dia(name: String, probs: Seq[(Cube, Double)]): Unit = {
        val probsIndexed = probs.sortBy((_, p) => -p).zipWithIndex
        val data = for (v <- probsIndexed) yield {
          val x = v._2.toDouble
          val y = v._1._2
          XY(x, y)
        }
        LineChartBuilder(f"sis-$name")
          .title(s"probabillities from $name")
          .xySeq(data)
          .xLabel("sector")
          .yLabel("probabillity")
          .yRangeMax(1.0)
          .xRangeMax(2000)
          .create()
      }

      Seq(
        CubeSplit.medium,
        CubeSplit.rough,
      ).foreach(cubeSplit => dia(cubeSplit.toString(), probsFromResource(cubeSplit)))
    }

    def directionColor(workPath: Path): Unit = {
      val bc = Color.veryDarkBlue

      def cos(dir: Vec) = {
        val w = Util.angle2DDeg(dir.x, dir.y)
        val min = 0.05
        val a = (1.0 - min) / 2.0
        val off = (2.0 + min) / 2.0
        val b = off + a * math.cos(degToRad(w))
        Color.red.brightness(b)
      }

      def coloredShape(dir: Vec): Shapable = {
        val dir1 = dir.copy(z = Random.nextDouble() * 1.0 - 0.5)
        val pos = Vec(x = Random.nextDouble() * 1.0 - 0.5, y = Random.nextDouble() * 1.0 - 0.5, z = 0.0)
        val c: Color = ImageUtil.colorFromDirection(dir.x, dir.y, palette = PaletteBrew.p1c10)
        Shapable.Cone(position = pos, direction = dir1, color = c, radius = 0.01)
      }

      val shapables: Seq[Shapable] = (0 to(360, 1))
        .map(d => PolarVec(1, degToRad(d), 0).toVec)
        .map(coloredShape)

      val file = outPath(workPath).resolve(s"tryout-dir-col.x3d")
      writeX3dFile1(bc, shapables, file)
    }

    def camRot(workPath: Path): Unit = {
      val bc = Color.veryDarkGreen
      //val shapables = ShapableFactory.sphereCoordinates(10)
      //val shapables = ImageFactory.sunms1(workPath, bc)
      //val shapables = ImageFactory.sund5(workPath, bc)
      val shapables = ImageFactory.sunms2(workPath, bc)
      val cams = cameras(100, 20, 15, eccentricity = 0.8, offset = Vec(5, 0, 0))(500)
      val duration = 60
      val xml: String = createCamAnimatedXml(shapables, cams, bc, duration, Rotation1(0, 0, 0))
      val file = outPath(workPath).resolve(s"tryout_cam_rot.x3d")
      gaia.Util.writeString(file, xml)
      println(s"wrote to $file")
    }

    def x3dDir(workPath: Path): Unit = {
      val bc = Color.darkBlue

      def combi(): Seq[Shapable] = {

        def colVecs(vecs: Seq[Vec], color: Color = Color.orange): Seq[(Vec, Color)] = {
          def brightnes(n: Int): Seq[Double] = {
            val k = 0.8 / n
            (0 until n).map(x => 1.0 - k * x)
          }

          val bs = brightnes(vecs.size)
            .map(b => color.brightness(b))

          vecs.zip(bs)
        }

        def brightVecs(vecs: Seq[Vec]): Seq[(Vec, Double)] = {
          def brightnes(n: Int): Seq[Double] = {
            val k = 0.8 / n
            (0 until n).map(x => 1.0 - k * x)
          }

          vecs.zip(brightnes(vecs.size))
        }

        def degs: Seq[Int] = {

          @tailrec
          def fd(v: Int, li: List[Int]): List[Int] = {
            val d = 1 + Random.nextInt(5)
            if v + d >= 360 then li
            else fd(v + d, (v + d) :: li)
          }

          fd(0, List())

        }

        def d1: Double = -80 + Random.nextInt(160)

        val vecs0 = (0 to(359, 1))
          .map(a => degToRad(a))
          .map(a => PolarVec(1, a, degToRad(d1)).toVec)

        val vecs1 = degs
          .map(a => degToRad(a))
          .map(a => PolarVec(1, a, degToRad(d1)).toVec)

        val vecs2 = Seq(181, 200, 300)
          .map(a => degToRad(a))
          .map(a => PolarVec(1, a, degToRad(d1)).toVec)


        val vecs = brightVecs(vecs1)

        val old = vecs
          .map { (v, bright) =>
            Shapable.Cylinder(position = Vec.zero, direction = v, radius = 0.005, color = Color.orange.brightness(bright))
          }
        val news = vecs
          .flatMap { (v, bright) =>
            val rot = vecToRotation(v)
            println(s"$v $rot")
            val d0 = Vec(-rot.axis.z, 0, rot.axis.x)
            val dir1 = d0.norm.mul(0.5)
            val dir2 = rot.axis.norm.mul(0.5)
            Seq(
              Shapable.CylinderRot(rotation = rot, radius = 0.003, height = 1.1, color = Color.yellow.brightness(bright)),
              //Shapable.Cylinder(position = Vec.zero, direction = dir1, radius = 0.01, color = Color.white.brightness(bright)),
              //Shapable.Cylinder(position = Vec.zero, direction = dir2, radius = 0.01, color = Color.white.brightness(bright)),
            )
          }
        old ++ news
      }


      val shapables =
        combi()
          ++ shapablesCoordinatesColored(3, bc)
      val file = outPath(workPath).resolve(s"tryout_x3dDir.x3d")
      writeX3dFile1(bc, shapables, file)
    }

    def sunMove(): Unit = {
      val sunPos = toGalacticCoords(Vec(0, 0, 0))
      println(s"sunpos: $sunPos")
      println(s"sundir: $galacticSunDir")

      val sun = StarPosDir(pos = Vec.zero, dir = Vec.zero)
      val gsun = toStarPosDirGalactic(sun)

      println(gsun)
    }

    def vecConvert(): Unit = {

      def adj(): Unit = {
        def ad(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

        val y = -0.0
        val x = ad(y)
        val v1 = "%7.2f %7.2f".format(y, x)
        println(v1)
      }

      def norm(): Unit = {
        val v = PolarVec(1.0000, 4.1416, 3.1316)
        val vn = v.adjust
        println(s"norm $v -> $vn")
      }

      /*
      [info] - vector convert reconvert pcp PolarVec(6.0,2.0,-3.0) *** FAILED ***
      [info]   "... 6.0000 |  5.1416 | [-]0.1416)" was not equal to "... 6.0000 |  5.1416 | [ ]0.1416)" (Tests.scala:82)
       */
      def pcp(): Unit = {
        val p1 = PolarVec(0, 0, 0)
        val p1a = p1.adjust
        val v1 = p1.toVec
        val pl2 = v1.toPolarVec
        println(s"pcp [${f(p1)} ${f(p1a)}] -> ${f(v1)} -> ${f(pl2)}")
      }

      def cpc(): Unit = {
        val v0 = Vec(0.0, 0.0, 1.0)
        val vp = v0.toPolarVec
        val v1 = vp.toVec
        println(s"cpc ${f(v0)} -> ${f(vp)} -> ${f(v1)}")
      }

      def round(): Unit = {
        def dround(x: Double) = {
          BigDecimal(x).setScale(13, BigDecimal.RoundingMode.HALF_UP).toDouble
        }

        for (scale <- 0 to 40) {
          val y = Random.nextDouble() * Random.nextInt(100)
          val x = dround(y)
          println("%22.18f".format(y))
          println("%22.18f".format(x))
        }
      }

      pcp()

    }


  }

  object Analyse {
    def amalyseStarsPerShell(workPath: Path): Seq[Unit] = {
      val shells = Seq(
        (" 1 kpc", 1.0, 1.1),
        (" 2 kpc", 2.0, 2.1),
        (" 3 kpc", 3.0, 3.1),
        (" 4 kpc", 4.0, 4.1),
        (" 5 kpc", 5.0, 5.1),
        (" 6 kpc", 6.0, 6.1),
        (" 7 kpc", 7.0, 7.1),
        (" 8 kpc", 8.0, 8.1),
        (" 9 kpc", 9.0, 9.1),
        ("10 kpc", 10.0, 10.1),
        ("11 kpc", 11.0, 11.1),
        ("12 kpc", 12.0, 12.1),
        ("13 kpc", 13.0, 13.1),
        ("14 kpc", 14.0, 14.1),
      )

      def filterBasic(star: Star)(minDistKpc: Double, maxDistKpc: Double): Option[Star] = {
        if (star.parallax <= 0) return None
        val dist = 1.0 / star.parallax
        if (dist < minDistKpc || dist > maxDistKpc) return None
        Some(star)
      }

      for ((id, min, max) <- shells) yield {
        val cnt = readBasic(workPath).flatMap(filterBasic(_)(min, max)).size
        println(s"$id - $cnt stars")
      }
    }

    def analyseDataNegativeParallax(workPath: Path): Unit = {
      var cntAll = 0
      var cntNegPar = 0
      var min = Double.MaxValue
      var max = Double.MinValue
      readBasic(workPath).foreach(s => {
        if (s.parallax <= 0) {
          cntNegPar += 1
          if (s.parallax < min) min = s.parallax
          if (s.parallax > max) max = s.parallax
        }
        cntAll += 1
      })
      val minDist = -1 / max
      val maxDist = -1 / min
      val perc = 100.0 * cntNegPar / cntAll
      println("=== negative parallax ===")
      println(f" $cntNegPar of $cntAll are negative parallaxes. $perc%.2f.")
      println(f" min parallax:  $min%20.2f            max parallax: $max%20.2f")
      println(f" max dist:      $minDist%20.3f kpc    min dist:     $maxDist%.3f kpc")
    }

    def analyseDataBasicSize(workPath: Path): Unit = {
      val size = readBasic(workPath).size
      println(s"Size of basic is $size")
    }

    def analyseDataBasicTop(workPath: Path): Unit = readBasic(workPath).take(20).foreach(println(_))

    def analyseDataHeader(): Unit = {
      val fn = "GaiaSource_1000172165251650944_1000424567594791808.csv.gz"
      val urlStr = s"http://cdn.gea.esac.esa.int/Gaia/gdr2/gaia_source/csv/$fn"
      println(s"downloading from $urlStr")
      val header = scala.io.Source.fromInputStream(GZIPInputStream(URL(urlStr).openStream()))
        .getLines()
        .take(1)
        .toSeq
      header.head
        .split(",")
        .zipWithIndex
        .foreach { case (nam, i) => println("%4d - %s".format(i, nam)) }
    }

  }

  object StarsFactory {

    object Cones {
      private def moveStar(s: Star, o: Vec): Star = {
        val pv = PolarVec(1 / s.parallax, degToRad(s.ra), degToRad(s.dec))
        val cv = pv.toVec
        val mv = cv.add(o)
        val pv1 = mv.toPolarVec
        Star(parallax = 1 / pv1.r, ra = radToDeg(pv1.ra), dec = radToDeg(pv1.dec),
          pmra = s.pmra, pmdec = s.pmdec, radialVelocity = s.radialVelocity)
      }

      private def cone(offset: Vec,
                       coneSteps: Int = 10, decSteps: Int = 20, raSteps: Int = 45,
                       properMovement: Double = 0.005): Seq[Star] = {
        for (w <- 0 to(360 - coneSteps, coneSteps);
             dec <- (-90 + decSteps) to(90 - decSteps, decSteps);
             ra <- 0 to(360 - raSteps, raSteps)) yield {
          val x = properMovement * math.sin(degToRad(w))
          val y = properMovement * math.cos(degToRad(w))
          val cstar = Star(ra, dec, 1.0 / 600, x, y, 100)
          moveStar(cstar, offset)
        }
      }

      def rich: Seq[Star] = {
        cone(Vec.zero)
          ++ cone(Vec(0, 1500, 0)) ++ cone(Vec(0, -1500, 0))
          ++ cone(Vec(0, 0, 1500)) ++ cone(Vec(0, 0, -1500))
      }

      def sparse: Seq[Star] = {
        cone(Vec.zero, coneSteps = 20, properMovement = 0.01)
      }

      def spikes: Seq[Star] = {
        val decSteps = 10
        val raSteps = 10
        for (dec <- (-90 + decSteps) to(90 - decSteps, decSteps);
             ra <- 0 to(360 - raSteps, raSteps)) yield {
          val dist = 600 + Random.nextInt(200)
          val velo = 50 + Random.nextInt(100)
          Star(ra, dec, 1.0 / dist, 0, 0, velo)
        }
      }
    }

    def polarToCartTest(workPath: Path): Seq[Star] = {
      for (t <- 0 to(355, 5); d <- -80 to(30, 1)) yield {
        val dist = 20 + Random.nextDouble() * 0.1
        Star(ra = t, dec = d, parallax = 1.0 / dist, 0, 0, 0)
      }
    }

    def cartToPolarTest(workPath: Path): Seq[Star] = {
      val vecs = Seq(
        (-20 to 20).map(v => Vec(v, 0, 10)),
        (-20 to 20).map(v => Vec(v, 0, -10)),
        (-20 to 20).map(v => Vec(v, 10, 0)),
        (-20 to 20).map(v => Vec(v, -10, 0)),
        (-20 to 20).map(v => Vec(0, v, 10)),
        (-20 to 20).map(v => Vec(0, v, -10)),
        (-20 to 20).map(v => Vec(10, v, 0)),
        (-20 to 20).map(v => Vec(-10, v, 0)),
      ).flatten
      for (vc <- vecs) yield {
        //println(vc)
        val v = vc.toPolarVec
        Star(ra = radToDeg(v.ra), dec = radToDeg(v.dec), parallax = 1.0 / v.r, 0, 0, 0)
      }
    }

  }

  private def writeX3dFile1(bc: Color, shapables: scala.Seq[Shapable], file: Path): Unit = {
    val xml = X3d.createXml(shapables, file.getFileName.toString, bc)
    gaia.Util.writeString(file, xml)
    println(s"wrote to $file")
  }

  private def f(prefix: String, a: Double, b: Double, c: Double): String = {
    def adj(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

    s"$prefix(%7.4f | %7.4f | %7.4f)".format(adj(a), adj(b), adj(c))
  }

  private def f(v: Vec): String = f("C", v.x, v.y, v.z)

  private def f(v: PolarVec): String = f("P", v.r, radToDeg(v.ra), radToDeg(v.dec))

}



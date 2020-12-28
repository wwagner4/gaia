package gaia

import gaia.Main.{GaiaImage, VideoConfig}
import gaia.X3d.Vec

import java.nio.file.{Files, Path}

object Automove {

  override def hashCode(): Int = super.hashCode()

  def createAutomove(gcfg: GaiaImage, workDir: Path) = {
    println(s"Automove: ${gcfg.id}")

    val cfg = gcfg.videoConfig.getOrElse(throw IllegalArgumentException("Illegal model id. No video config defined"))
    for (moveCfg <- cfg.moves) {
      val modelsDir = workDir.resolve("models")
      val videosDir = workDir.resolve("videos")
      if (Files.notExists(videosDir)) Files.createDirectories(videosDir)
      val modelFile = modelsDir.resolve(s"${gcfg.id}.x3d").toAbsolutePath.toFile
      val rotAxes = moveCfg.rotationAxis.toVec
      val rotCart = moveCfg.rotation.toVec
      println(s"-- rotChat: ${moveCfg.rotation} -> $rotCart")
      val content =
        s"""
          |<X3D  profile='Immersive' version='3.4' >
          |  <Scene>
          |    <WorldInfo title='${gcfg.id} ${moveCfg.id}'/>
          |    <Background skyColor='${gcfg.backColor.strNoComma}'/>
          |    <Viewpoint	position='0 0 ${moveCfg.viewpointDistance}' description='gaia_vp'/>		
          |    <Transform rotation='1 0 0 ${rotCart.x}'>
          |     <Transform rotation='0 1 0 ${rotCart.y}'>
          |      <Transform rotation='0 0 1 ${rotCart.z}'>
          |       <Transform DEF='object'>
          |     	  <Inline bboxCenter='0 0 0' bboxSize='-1 -1 -1' url='file:///$modelFile'/>
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
      val moveFile = modelsDir.resolve(moveFileName)
      Util.writeString(moveFile, content)

      val videoFileName = s"$moveFileBase.mp4"
      val videoFile = videosDir.resolve(videoFileName).toAbsolutePath.toString
      println(s"Wrote to ${moveFile.toAbsolutePath}")
      println("-- bash -----------------------------------------------------------------------------------------")
      println(f"view3dscene ${moveFile.toAbsolutePath} --geometry ${cfg.resolution.resString} --screenshot-range 0 ${cfg.frameRate.seconds} ${cfg.frameCount} $videoFile")
      println("-------------------------------------------------------------------------------------------------")
    }
  }
}

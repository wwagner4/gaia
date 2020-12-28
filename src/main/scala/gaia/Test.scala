package gaia

import gaia.Main.RotAxesDeg
import gaia.X3d.{PolarVecDeg, Vec}

import java.nio.file.Path

object Test {

  def test(args: List[String], workPath: Path): Unit = {
    println(s"Some tests ${workPath.toAbsolutePath} ${args.mkString("[", ",", "]")}")
    
    val rs = Seq(
      RotAxesDeg(0, 0),
      RotAxesDeg(0, 90),
      RotAxesDeg(90, 0),
    )
    
    rs.map(r => (r, r.toVec)).foreach{case (a, b) => println(s"$a -> $b")}

    val nearEcl = Vec(0, 45, 30).toPolarVec
    val ra = X3d.radToDeg(nearEcl.ra)
    val dec = X3d.radToDeg(nearEcl.dec)
    println(s"steep: $ra, $dec")
    
  }



}

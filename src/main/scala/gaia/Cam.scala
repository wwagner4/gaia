package gaia

object Cam {

  import Vector._

  case class Camera(
                     name: String,
                     pos: Vec,
                     dir: Vec,
                   )

  def degSteps(n: Int): Seq[Double] = {
    val d = 360.0 / n

    def rs(x: Double, res: List[Double]): List[Double] = {
      if x >= 360 then res
      else rs(x + d, x :: res)
    }

    rs(0, List.empty[Double]).reverse
  }

  def cameras(degStep: Int, ra: Int, dec: Int, radius: Double, name: String = "gaiadefinded"): Seq[Camera] = {
    degSteps(degStep)
      .zip(LazyList.continually(PolarVec(1, 0, 0)))
      .map { case (d, v) => v.copy(ra = degToRad(d)) }
      .map(pv => pv.toVec)
      .map(v => v.roty(degToRad(dec)))
      .map(v => v.rotz(degToRad(ra)))
      .zipWithIndex
      .map { case (v, i) =>
        val nam = f"${name}_${i}%04d"
        Camera(nam, v, v.sub(Vec.zero)) }
  }


}

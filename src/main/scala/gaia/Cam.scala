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

  def cameras(ra: Int, dec: Int, radius: Double, name: String = "gaiadefined")(steps: Int): Seq[Camera] = {
    degSteps(steps)
      .zip(LazyList.continually(PolarVec(radius, 0, 0)))
      .map { case (d, v) => v.copy(ra = degToRad(d)) }
      .map(pv => pv.toVec)
      .map(v => v.roty(degToRad(dec)))
      .map(v => v.rotz(degToRad(ra)))
      .zipWithIndex
      .map { case (v, i) =>
        val nam = f"${name}_${i}%04d"
        val dir = Vec.zero.sub(v)
        Camera(nam, v, dir) }
  }


}

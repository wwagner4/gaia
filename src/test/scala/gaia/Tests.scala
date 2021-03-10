package gaia

import gaia.X3d.{PolarVec, Vec, pihalbe}

import java.util.Locale
import org.scalatest._
import funsuite._
import matchers._

class Tests extends AnyFunSuite with must.Matchers {

  val delta = 0.000001

  def f(a: Double, b: Double, c: Double) = "%7.4f | %7.4f | %7.4f".format(a, b, c)

  def f(v: Vec): String = f(v.x, v.y, v.z)

  def f(v: PolarVec): String = f(v.r, v.ra, v.dec)

  val VEC_POLAR = Seq(
    (Vec(1, 0, 0), 1.0, 0.0, 0.0),
    (Vec(0, 1, 0), 1.0, X3d.pihalbe, 0.0),
    (Vec(0, 0, 1), 1.0, 0.0, X3d.pihalbe),
  )

  for ((v, r, ra, dec) <- VEC_POLAR) {
    test(s"vec to polar vec $v") {
      val vp = v.toPolarVec
      f(vp) mustBe f(r, ra, dec)
    }
  }

  val VECS = Seq(
    (PolarVec(1, 0, 0), 1.0, 0.0, 0.0),
    (PolarVec(1, X3d.pihalbe, 0), 0.0, 1.0, 0.0),
    (PolarVec(1, 0, X3d.pihalbe), 0.0, 0.0, 1.0),
  )

  for ((pv, x, y, z) <- VECS) {
    test(s"to cart vec $pv") {
      val v = pv.toVec
      f(v) mustBe f(x, y, z)
    }
  }


  val VEC_CONV = Seq(
    Vec(1, 2, 3),
    Vec(1, -2, 3),
    Vec(-1, -2, 3),
    Vec(-1, -2, -3),
    Vec(-1, 2, -3),
    Vec(0, 0, 1),
  )

  for v <- VEC_CONV do {
    test(s"vector convert reconvert ${v}") {
      val vp = v.toPolarVec
      val v1 = vp.toVec
      f(v1) mustBe f(v)
    }
  }


  val VEC_SUB = Seq(
    (Vec(3, 2, 0), Vec(1, 2, 0), Vec(2, 0, 0)),
    (Vec(0, 4, 1), Vec(0, 1, 2), Vec(0, 3, -1)),
  )

  for ((star, gc, should) <- VEC_SUB) {
    test(s"subtract vecor $star") {
      star.sub(gc) === should
    }
  }

  lazy val ic = ImageUtil.inCube(8, 2) _

  val IN_CUBE = Seq(
    (Vec(0.5, 0.5, 0.5), (0, 0, 0), true),
    (Vec(0.5, 0.5, 0.5), (1, 0, 0), false),
    (Vec(0.5, 0.5, 0.5), (-1, -1, 0), false),
    (Vec(0.5, 0.5, 0.5), (-2, -1, 0), false),
    (Vec(0.5, 0.5, 0.5), (0, -1, 0), false),
    (Vec(0.5, 0.5, 0.5), (0, -1, -2), false),
    (Vec(0.5, -0.5, 0.5), (0, -1, 0), true),
    (Vec(0.5, -0.5, 0.5), (1, -1, 0), false),
    (Vec(0.5, -0.5, 0.5), (-1, -1, 0), false),
    (Vec(0.5, -0.5, 0.5), (-2, -1, 0), false),
    (Vec(0.5, -0.5, 0.5), (0, -1, -1), false),
    (Vec(0.5, -0.5, 0.5), (0, -1, -2), false),
  )


  for ((v, (i, j, k), result) <- IN_CUBE) {
    test(s"in out a cube ${v} $i $j $k") {
      ic(v, i, j, k) == result
    }
  }
}
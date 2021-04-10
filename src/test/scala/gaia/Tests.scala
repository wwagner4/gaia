package gaia

import java.util.Locale
import org.scalatest._
import funsuite._
import matchers._

class Tests extends AnyFunSuite with must.Matchers {

  import Vector._
  import Cam._

  val delta = 0.000001

  def f(prefix: String, a: Double, b: Double, c: Double) = {
    def adj(v: Double): Double = if (v <= 0.0 && v > -0.0000001) 0.0 else v

    s"$prefix(%7.4f | %7.4f | %7.4f)".format(adj(a), adj(b), adj(c))
  }

  def f(v: Vec): String = f("C", v.x, v.y, v.z)

  def f(v: PolarVec): String = f("P", v.r, v.ra, v.dec)

  val VEC_CONV_CPC = {
    val v1 = for (x <- -2 to 2;
                  y <- -2 to 2;
                  z <- -2 to 2) yield
      Vec((x + 0.8768) * 0.123, (y + 0.001) * 0.123123, z * 2.4)
    val v2 = Seq(
      Vec(1, 2, 3),
      Vec(1, 2, -3),
      Vec(1, -2, 3),
      Vec(-1, -2, 3),
      Vec(-1, -2, -3),
      Vec(-1, 2, -3),
      Vec(0, 0, 1),
      Vec(-3, -2, 3),
      Vec(-3, -2, -3),
      Vec(-3, 2, -3),
      Vec(3, 0, 1),
    )
    v1 ++ v2
  }

  for v <- VEC_CONV_CPC do {
    test(s"vector convert reconvert ${v}") {
      val vp = v.toPolarVec
      val v1 = vp.toVec
      f(v1) mustBe f(v)
    }
  }

  val VEC_CONV_PCP = Seq(
    PolarVec(1, 0.0, 0.0),
    PolarVec(1, 1.0, 0.01),
    PolarVec(1, 2.0, 0.01),
    PolarVec(1, 3.0, 0.01),
    PolarVec(1, 0.0, 1.0),
    PolarVec(1, 0.0, 2.0),
    PolarVec(1, 0.0, 3.0),
    PolarVec(1, 2.0, 1.0),
    PolarVec(1, 2.0, 2.0),
    PolarVec(1, 2.0, 3.0),
    PolarVec(10, 0.0, 0.0),
    PolarVec(14, 1.0, 0.0),
    PolarVec(15, 2.0, 0.0),
    PolarVec(3, 3.0, 0.0),
    PolarVec(3, -3.0, 0.0),
    PolarVec(4, 0.0, 1.0),
    PolarVec(2, 0.0, 2.0),
    PolarVec(7, 0.0, 3.0),
    PolarVec(3, 2.0, 1.0),
    PolarVec(4, 2.0, 2.0),
    PolarVec(6, 2.0, 3.0),
    PolarVec(6, -2.0, 3.0),
    PolarVec(6, 2.0, -3.0),
    PolarVec(6, -2.0, -3.0),
    PolarVec(1, 0, degToRad(100)),
    PolarVec(1, degToRad(100), 0),
  )

  for v <- VEC_CONV_PCP do {
    test(s"vector convert reconvert pcp ${v}") {
      val va = v.adjust
      val vc = v.toVec
      val v1 = vc.toPolarVec
      println(s"-- ${f(v)} --> ${f(vc)} --> ${f(v1)}")
      println(s"-- ${v} --> ${vc} --> ${v1}")
      f(v1) mustBe f(va)
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

  val degStepsTestVals = Seq(
    (2, List(0.0, 180.0)),
    (3, List(0.0, 360.0/ 3, 2.0 * 360.0/ 3)),
    (4, List(0.0, 90.0, 180.0, 270.0)),
  )

  for (n, l) <- degStepsTestVals do {
    def f(vals: Iterable[Double]): String = {
      vals.map(v => "%.5f".format(v)).mkString("|")
    }

    test(s"360 steps ${n}") {
      val l1 = degSteps(n, false)
      f(l1) mustBe f(l)
    }
  }

  val degStepsReverseTestVals = Seq(
    (2, List(360.0, 180.0)),
    (4, List(360.0, 270.0, 180.0, 90.0)),
  )

  for (n, l) <- degStepsReverseTestVals do {
    def f(vals: Iterable[Double]): String = {
      vals.map(v => "%.5f".format(v)).mkString("|")
    }

    test(s"360 steps reverse ${n}") {
      val l1 = degSteps(n, true)
      f(l1) mustBe f(l)
    }
  }

  test("intervals") {
    def fiv(i: (Double, Double)): String = {
      "(%.5f, %.5f)".format(i._1, i._2)
    }
    val ivs = Util.intervals(3, 0, 9)
    ivs.map(fiv).mkString(", ") mustBe Seq((0.0, 3.0), (3.0, 6.0), (6.0, 9.0)).map(fiv).mkString(", ") 
  }
  
}
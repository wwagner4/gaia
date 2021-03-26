package gaia


import gaia.Data.Star

import java.nio.file.Path
import scala.util.Random


object Vector {

  val pi = math.Pi
  val pidiv2 = math.Pi / 2.0
  val pimul2 = math.Pi * 2.0

  private val degRad = 180.0 / pi

  def degToRad(deg: Double): Double = deg / degRad

  def radToDeg(rad: Double): Double = rad * degRad

  object Vec {
    def zero: Vec = Vec(0, 0, 0)
  }

  case class Vec(x: Double, y: Double, z: Double) {

    def strComma = s"$x, $y, $z"

    def strNoComma = s"$x $y $z"

    def mul(factor: Double): Vec = Vec(x * factor, y * factor, z * factor)

    def add(other: Vec): Vec = Vec(x + other.x, y + other.y, z + other.z)

    def sub(other: Vec): Vec = Vec(x - other.x, y - other.y, z - other.z)

    def rotx(a: Double): Vec =
      Vec(
        x,
        y * math.cos(a) - z * math.sin(a),
        y * math.sin(a) + z * math.cos(a),
      )

    def roty(a: Double): Vec =
      Vec(
        x * math.cos(a) + z * math.sin(a),
        y,
        -x * math.sin(a) + z * math.cos(a),
      )

    def rotz(a: Double): Vec =
      Vec(
        x * math.cos(a) - y * math.sin(a),
        x * math.sin(a) + y * math.cos(a),
        z,
      )

    lazy val length: Double = math.sqrt((x * x) + (y * y) + (z * z))

    def norm: Vec = {
      val l = length
      Vec(x / l, y / l, z / l)
    }

    def sprod(other: Vec): Double = (x * other.x) + (y * other.y) + (z * other.z)

    def angle(other: Vec): Double = math.acos(sprod(other) / (length * other.length)) * 180 / pi

    def toPolarVec: PolarVec = {
      def atan2: Double = {
        if (x > 0.0) math.atan(y / x)
        else if (x < 0.0) {
          if (y >= 0.0) math.atan(y / x) + pi
          else math.atan(y / x) - pi
        }
        else {
          math.signum(y) * pidiv2
        }
      }

      val r = math.sqrt(x * x + y * y + z * z)
      if (r == 0.0) PolarVec(0.0, 0.0, 0.0)
      else {
        val dec = math.acos(z / r)
        val ra = atan2
        val dec1 = pidiv2 - dec
        PolarVec(r, ra, dec1).adjust
      }
    }

    def toVec4(u: Double = 0.0) = Vec4(x, y, z, u)
  }

  case class PolarVec(r: Double, ra: Double, dec: Double) {
    def toVec: Vec = {
      val dec1 = pidiv2 - dec
      val x = r * math.sin(dec1) * math.cos(ra)
      val y = r * math.sin(dec1) * math.sin(ra)
      val z = r * math.cos(dec1)
      Vec(x, y, z)
    }

    def adjust: PolarVec = {

      def adjustRa(v: Double): Double = {
        val v1 = v % pimul2
        if (v1 < 0.0) v1 + pimul2 else v1
      }

      val dec1 = dec % pi
      if (dec1 > pidiv2) {
        val ra1 = adjustRa(ra + pi)
        val dec2 = pi - dec1
        PolarVec(r, ra1, dec2)
      }
      else if (dec1 < -pidiv2) {
        val ra1 = adjustRa(ra + pi)
        val dec2 = - dec1 - pi
        PolarVec(r, ra1, dec2)
      }
      else {
        val ra1 = adjustRa(ra)
        PolarVec(r, ra1, dec1)
      }
    }
  }

  case class Vec2(x: Double, y: Double) {

    def strComma = s"$x, $y"

    def strNoComma = s"$x $y"
  }

  case class Vec4(x: Double, y: Double, z: Double, u: Double) {

    def strComma = s"$x, $y, $z, $u"

    def strNoComma = s"$x $y $z $u"
  }



}

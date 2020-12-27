package gaia

import gaia.X3d.{PolarVec, Vec}
import org.junit.Assert._
import org.junit.Test

class Tests {

  val delta = 0.0001

  @Test
  def vectorConversionA(): Unit = {
    val v = Vec(1, 2, 3)
    val vp = v.toPolarVec
    val v1 = vp.toVec
    assertEquals(v.x, v1.x, delta)
    assertEquals(v.y, v1.y, delta)
    assertEquals(v.z, v1.z, delta)
  }

  @Test
  def vectorConversionB(): Unit = {
    val v = Vec(1, -2, 3)
    val vp = v.toPolarVec
    val v1 = vp.toVec
    assertEquals(v.x, v1.x, delta)
    assertEquals(v.y, v1.y, delta)
    assertEquals(v.z, v1.z, delta)
  }

  @Test
  def vectorConversionC(): Unit = {
    val v = Vec(-1, -2, 3)
    val vp = v.toPolarVec
    val v1 = vp.toVec
    assertEquals(v.x, v1.x, delta)
    assertEquals(v.y, v1.y, delta)
    assertEquals(v.z, v1.z, delta)
  }

  @Test
  def vectorConversionD(): Unit = {
    val v = Vec(-1, -2, -3)
    val vp = v.toPolarVec
    val v1 = vp.toVec
    assertEquals(v.x, v1.x, delta)
    assertEquals(v.y, v1.y, delta)
    assertEquals(v.z, v1.z, delta)
  }

  @Test
  def vectorConversionE(): Unit = {
    val v = Vec(-1, 2, -3)
    val vp = v.toPolarVec
    val v1 = vp.toVec
    assertEquals(v.x, v1.x, delta)
    assertEquals(v.y, v1.y, delta)
    assertEquals(v.z, v1.z, delta)
  }

}
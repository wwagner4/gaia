package gaia

import gaia.X3d.{PolarVec, Vec}
import org.junit.Assert._
import org.junit.Test

import java.util.Locale

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

  @Test
  def convertToGalacticCoordinates(): Unit = {
    val gc = Vec(1, 2, 0)
    val star = Vec(3, 2, 0)

    val s1 = star.sub(gc)

    assertEquals(Vec(2, 0, 0), s1)
  }

  @Test
  def convertToGalacticCoordinatesXY(): Unit = {
    val gc = Vec(1, 2, 0)
    val star = Vec(3, 2, 0)

    val s1 = star.sub(gc)

    assertEquals(Vec(2, 0, 0), s1)
  }

  @Test
  def convertToGalacticCoordinatesZY(): Unit = {
    val gc = Vec(0, 1, 2)
    val star = Vec(0, 4, 1)

    val s1 = star.sub(gc)

    assertEquals(Vec(0, 3, -1), s1)
  }

  @Test
  def toGalacticNearTheCenter(): Unit = {
    val s = Vec(0.4, -6, -3)
    val s1 = ImageUtil.toGalacticPos(s)
    assertEquals("[0.8579,0.9860,0.8711]", f(s1))
  }

  def f(v: Vec): String = "[%.4f,%.4f,%.4f]".formatLocal(Locale.ENGLISH, v.x, v.y, v.z)

  lazy val ic = ImageUtil.inCube(8, 2)_

  @Test
  def inCube01(): Unit = {

    assertTrue(ic(Vec(0.5, 0.5, 0.5), 0, 0, 0))

    assertFalse(ic(Vec(0.5, 0.5, 0.5), 1, 0, 0))
    assertFalse(ic(Vec(0.5, 0.5, 0.5), -1, -1, 0))
    assertFalse(ic(Vec(0.5, 0.5, 0.5), -2, -1, 0))
    assertFalse(ic(Vec(0.5, 0.5, 0.5), 0, -1, 0))
    assertFalse(ic(Vec(0.5, 0.5, 0.5), 0, -1, -2))
  }

  @Test
  def inCube02(): Unit = {

    assertTrue(ic(Vec(0.5, -0.5, 0.5), 0, -1, 0))

    assertFalse(ic(Vec(0.5, -0.5, 0.5), 1, -1, 0))
    assertFalse(ic(Vec(0.5, -0.5, 0.5), -1, -1, 0))
    assertFalse(ic(Vec(0.5, -0.5, 0.5), -2, -1, 0))
    assertFalse(ic(Vec(0.5, -0.5, 0.5), 0, -1, -1))
    assertFalse(ic(Vec(0.5, -0.5, 0.5), 0, -1, -2))
  }
}
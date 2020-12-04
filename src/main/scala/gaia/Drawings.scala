package gaia

object Drawings {

  import X3d._

  def drawFadingLines(backColor: Color): Seq[Shapable] = {

    def ranDist(shape: (Vec) => Seq[Shapable]): Seq[Shapable] = {
      (0 to 10)
        .flatMap { _ =>
          val off = Vec(Util.ranOff(0.1), Util.ranOff(10), Util.ranOff(10))
          shape(off)
        }
    }

    def lines(off: Vec): Seq[Shapable] = {
      val c = Color.white
      val f = 0.5
      (1 to 20)
        .map(i => i / f)
        .map { t =>
          val scaling = (0.1 + t)
          val pos = Vec(off.x, math.sin(t) + off.y, math.cos(t) + off.z)
          Shapable.Line(startColor = c, endColor = backColor, translation = pos, scaling = 10)
        }
    }

    ranDist(lines)
  }

  def drawCylinderRotation(bgColor: Color): Seq[Shapable] = {


    def cylinders(color: Color, off: Vec): Seq[Shapable] = {
      (1 to 30)
        .map(i => i * 0.1)
        .map { t =>
          val pos = Vec(off.x + t, off.y, off.z)
          val rot = Vec(0.5 * t, 0, 0)
          Shapable.Cylinder(translation = pos, color = color, radius = 0.01, height = 1.7, rotaion = rot)
        }
    }

    val offs = Seq(
      (Color.orange, Vec(0, 0, 0)),
      (Color.yellow, Vec(1, 0, 0)),
      (Color.red, Vec(0, 1, 0)),
      (Color.green, Vec(0, 0, 1)),
      (Color.blue, Vec(1, 1, 1)),
    )
    offs.flatMap { case (c, off) => cylinders(c, off) }
  }

  def drawLinesRotation(bgColor: Color): Seq[Shapable] = {
    
    def lines(color: Color, off: Vec): Seq[Shapable] = {
      (0 to 500)
        .map(i => i * 0.1)
        .map { t =>
          val color = Color.random
          val pos = Vec(off.x, off.y, off.z)
          val rot = Vec(Util.ranOff(6.3), 0, Util.ranOff(6.3))
          Shapable.Line(translation = pos, rotaion = rot, startColor = Color.white, endColor = bgColor, scaling = 20.0 + Util.ranOff(2))
        }

    }

    (0 to 20)
      .map {
        t =>
          val c = Color.random
          val offx = Util.ranOff(20)
          val offy = Util.ranOff(20)
          val offz = Util.ranOff(20)
          (c, Vec(offx, offy, offz))
      }
      .flatMap { case (color, off) => lines(color, off) }
  }

  def drawLinesSimpleRot(bgColor: Color): Seq[Shapable] = {

    def lines(off: Vec): Seq[Shapable] = {
      (0 to 300)
        .map(i => i * 0.01)
        .map { t =>
          val pos = Vec(off.x, off.y + t, off.z)
          val rot = Vec(0, Util.ranOff(7), 0)
          Shapable.Line(translation = pos, rotaion = rot, startColor = Color.yellow, endColor = Color.orange,
            scaling = 50.0)
        }

    }

    val offs = Seq(
      Vec(-2, 0, 0),
      Vec(-2, 0, 2),
      Vec(-2, 0, 4),
      Vec(2, 0, 0),
      Vec(2, 0, 2),
      Vec(2, 0, 4),
      Vec(0, 0, 0),
      Vec(0, 0, 2),
      Vec(0, 0, 4),
    )

    offs.flatMap(o => lines(o))
  }

  def drawLinesSimpleScale(bgColor: Color): Seq[Shapable] = {

    def lines(off: Vec): Seq[Shapable] = {
      (0 to 60)
        .map(i => i * 0.1)
        .map { t =>
          val pos = Vec(off.x, off.y, off.z)
          val rot = Vec(Util.ranOff(7), 0, Util.ranOff(7))
          Shapable.Line(translation = pos, rotaion = rot, startColor = Color.yellow, endColor = Color.darkRed,
            scaling = 1 + t * 0.01)
        }

    }


    val offs = (1 to 10)
      .map(_ => Vec(Util.ranOff(3), Util.ranOff(3), Util.ranOff(3)))

    offs.flatMap(o => lines(o))
  }

}


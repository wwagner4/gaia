package gaia

import entelijan.viz.Viz

import java.nio.file.Path

object DiagramFactory {

  import DataUtil._

  def gcs2(workDir: Path): Viz.Dia[Viz.XY] = {
    val radius = 20

    val regions = Util.intervals(16, -3, 3)
      .zipWithIndex
      .map { case ((z1, z2), i) => Cylinder(f"a$i%03d", radius, z1, z2) }

    val starsFiltered = DataUtil.starsAroundGalacticCenter(workDir, 1)

    val diagrams = starsPerRegion(starsFiltered, regions)
      .map((r, stars) => (r, DataUtil.starsPerSectorEqualized(stars, 20).flatten))
      .map((r, stars) => (r, stars.map(s => Viz.XY(s.pos.x, s.pos.y))))
      .map((region, data) => Viz.Diagram[Viz.XY](
        id = region.id,
        title = f"Stars in ${region.toString}",
        dataRows = Seq(Viz.DataRow[Viz.XY](
          data = data,
          style = Viz.Style_DOTS))))

    Viz.MultiDiagram[Viz.XY](
      id = "gcs1",
      columns = 4,
      width = 1300,
      height = 1300,
      title = Some("Galaxy in Slices"),
      diagrams = diagrams,
    )
  }

}

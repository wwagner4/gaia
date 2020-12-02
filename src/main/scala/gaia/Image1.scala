package gaia

import java.nio.file.Files

/*
Dataanalyse basic
ra: 0 - 360
dec: -90 +90
parallaxe: -51.55414944640443 / 304.21902198647064

explore parallaxe
histo: -60 -- 310. cnt / range / width. 20 / 370.0 / 18.5

questions / TODOs :
- how many parallaxes are negative ?
  71149 of 7183262 parallaxes are negative. This is 0.99 %
- how to calculate distance
- what mean the meaned distance compared to the size of the milkiway and 
  relative to the location of the solarsystem in the milkiway?
- move these informations to readme.
 */

object Image1 {

  def draw(): Unit = {
    
    println("Drawing image 1")

    var ncnt = 0
    var cnt = 0
    Data
      .readBasic.map(s => s.parallax)
      .foreach{p => 
        cnt += 1
        if (p <= 0) ncnt += 1         
      }
      val rel = "%.2f".format(100.0 * ncnt / cnt)
      print(s"$ncnt of $cnt parallaxes are negative. This is $rel %")
    
    

  }
  
  def prepare: Unit = {
    val imagePath = Util.datapath.resolve("image1")
    if !Files.exists(imagePath)
      Files.createDirectories(imagePath)
    println(s"Image directory $imagePath exists")
  }

  def range: Unit = {
    val size = 3_000_000
    val dat = Data.readBasic.map(s => s.parallax).toSeq
    val max = dat.max
    val min = dat.min
    println(s"min/max for $size. $min / $max")
  }


  def findHistoBorders: Unit = {
    val cnts = 8 to 30
    for (cnt <- cnts) {
      val rng = 310.0 + 60
      val width = rng / cnt
      println(s"cnt / range / width. $cnt / $rng / $width")
    }
  }
}

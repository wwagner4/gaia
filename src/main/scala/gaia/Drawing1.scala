package gaia

import java.nio.file.Files
/*
Dataanalyse basic
ra: 0 - 360
dec: -90 +90
parallaxe: -51.55414944640443 / 304.21902198647064

explore parallaxe
histo: -60 -- 310. cnt / range / width. 20 / 370.0 / 18.5


 */

class Counter(val id: Int) 
    var cnt = 0
  
object Drawing1 {
  
  def draw(): Unit = {
    
    println("Drawing image 1")
    
    
    
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
      val rng= 310.0 + 60
      val width = rng / cnt
      println(s"cnt / range / width. $cnt / $rng / $width")
    }
  }
}

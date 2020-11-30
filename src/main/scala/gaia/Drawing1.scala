package gaia

import java.nio.file.Files

object Drawing1 {
  
  def draw(): Unit = {
    
    println("Drawing image 1")
    val imagePath = Util.datapath.resolve("image1")
    if !Files.exists(imagePath)
      Files.createDirectories(imagePath)
    println(s"Image directory $imagePath exists")
    
  }

}

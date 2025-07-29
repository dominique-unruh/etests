package utils

import os.Path
import sourcecode.Enclosing

import java.nio.file.Files
import scala.collection.mutable

object Utils {
  def uniqueMap[K,V](elems: (K,V)*): Map[K, V] =
    val set = new mutable.HashSet[K]
    for (k <- elems.map(_._1))
      assert(!set.contains(k))
      set += k
    Map(elems*)


  private lazy val tempDir = {
    val dir = os.temp.dir(prefix = "assessments", deleteOnExit = true)
    println(s"Temp directory: $dir")
    dir
  }
  def getTempDir(implicit enclosing: Enclosing): Path = os.temp.dir(dir = tempDir, prefix = enclosing.value, deleteOnExit = false)
}

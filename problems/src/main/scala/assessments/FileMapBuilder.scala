package assessments

import utils.Utils

import scala.collection.mutable
import scala.util.boundary
import scala.util.boundary.break

trait FileMapBuilder {
  def hasName(name: String): Boolean

  def result(): Map[String, (String, Array[Byte])]

  /** Add a filename with a given mime type and content. Returns the path/URL to the added file */
  def add(filename: String, mimeType: String, content: Array[Byte]): String = {
    val (basename, extension) = Utils.splitExtFilename(filename)
    add(basename = basename, extension = extension, mimeType = mimeType, content = content)
  }

  def add(basename: String, extension: String, mimeType: String, content: Array[Byte]): String
}

class DefaultFileMapBuilder(prefix: String) extends FileMapBuilder {
  private val map = mutable.Map[String, (String, Array[Byte])]()

  final def hasName(name: String): Boolean = map.contains(name)

  final def result(): Map[String, (String, Array[Byte])] = map.toMap

  final def add(basename: String, extension: String, mimeType: String, content: Array[Byte]): String = {
    def freshName: String = boundary {
      val name = s"$basename.$extension"
      if (!hasName(name))
        break(name)
      for (i <- 1 until Int.MaxValue) {
        val namei = s"$basename$i.$extension"
        if (!hasName(namei))
          break(namei)
      }
      assert(false) // Unreachable unless there are Int.MaxValue many files
    }

    val name = freshName
    map.put(name, (mimeType, content))
    prefix + name
  }
}

class DataUrlFileMapBuilder extends FileMapBuilder {
  override def hasName(name: String): Boolean = false
  override def result(): Map[String, (String, Array[Byte])] = Map.empty
  override def add(basename: String, extension: String, mimeType: String, content: Array[Byte]): String =
    Utils.dataUrl(mimeType, content)
}
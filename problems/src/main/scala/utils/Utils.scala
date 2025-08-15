package utils

import com.typesafe.scalalogging.Logger
import sourcecode.Enclosing

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, StringSelection}
import java.io.{BufferedReader, FileReader}
import java.nio.file.{Files, Path, Paths}
import java.util.{Base64, Properties}
import scala.jdk.CollectionConverters.given
import scala.collection.mutable
import scala.util.Using

object Utils {
  private val logger = Logger[Utils.type]

  private var systemPropertiesLoaded = false
  def loadSystemProperties(): Unit = synchronized {
    if (!systemPropertiesLoaded) {
      val path = os.pwd / "java.properties"
      if (os.exists(path)) {
        logger.debug(s"Loading properties from $path")
        val props = new Properties()
        Using.resource(path.getInputStream) { stream => props.load(stream) }
        for ((key, value) <- props.asScala)
          System.setProperty(key, value)
      }
      systemPropertiesLoaded = true
    }
  }

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
  def getTempDir(implicit enclosing: Enclosing): Path = os.temp.dir(dir = tempDir, prefix = enclosing.value, deleteOnExit = false).toNIO
  
  def dataUrl(mimeType: String, data: Array[Byte]): String = {
    val base64 = Base64.getEncoder.encodeToString(data)
    s"data:image/png;base64,$base64"
  }
  
  def copyStringToClipboard(string: String): Unit = {
    val stringSelection = new StringSelection(string)
    val clipboard: Clipboard = Toolkit.getDefaultToolkit.getSystemClipboard
    clipboard.setContents(stringSelection, null)
  }

  private val stripLeadingEmptyLinesRegex = """(?s)^([ \t]*\n)+""".r
  def stripLeadingEmptyLines(string: String): String =
    stripLeadingEmptyLinesRegex.replaceFirstIn(string, "")
  private val stripTrailingEmptyLinesRegex = """(?s)(\n[ \t]*)+$""".r
  def stripTrailingEmptyLines(string: String, keepFinalNewline: Boolean = false): String =
    stripTrailingEmptyLinesRegex.replaceFirstIn(string, if (keepFinalNewline) "\n" else "")
  def stripLeadingTrailingEmptyLines(string: String, keepFinalNewline: Boolean = false): String =
    stripLeadingEmptyLines(stripTrailingEmptyLines(string, keepFinalNewline = keepFinalNewline))


  private val escapeTeXMap = Map(
    '\\' -> "\\textbackslash{}",
    '{' -> "\\{",
    '}' -> "\\}",
    '$' -> "\\$",
    '&' -> "\\&",
    '%' -> "\\%",
    '#' -> "\\#",
    '^' -> "\\textasciicircum{}",
    '_' -> "\\_",
    '~' -> "\\textasciitilde{}"
  )
  def escapeTeX(string: String): String = {
    // Based on Claude AI
    val sb = new StringBuilder()
    string.foreach { c =>
      sb.append(escapeTeXMap.getOrElse(c, c.toString))
    }
    sb.toString()
  }

  def getSystemPropertyPath(property: String, fileDescription: String): Path = {
    val path = System.getProperty(property)
    if (path == null)
      throw new RuntimeException(s"Please configure $property in java.properties (path to $fileDescription)")
    val path2 = Path.of(path)
    if (!path2.isAbsolute)
      throw new RuntimeException(s"$property in java.properties must refer to an absolute path")
    path2
  }
}

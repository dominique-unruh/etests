package utils

import os.Path
import sourcecode.Enclosing

import java.awt.Toolkit
import java.awt.datatransfer.{Clipboard, StringSelection}
import java.nio.file.Files
import java.util.Base64
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
}

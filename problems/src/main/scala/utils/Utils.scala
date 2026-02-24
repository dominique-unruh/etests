package utils

import assessments.{ExceptionContext, ExceptionWithContext}
import com.microsoft.playwright.{Page, Playwright}
import com.typesafe.scalalogging.Logger
import sourcecode.{Enclosing, FileName}

import java.awt.{GridBagConstraints, GridBagLayout, Insets, Toolkit}
import java.awt.datatransfer.{Clipboard, StringSelection}
import java.io.{BufferedReader, FileReader}
import java.nio.file.{Files, Path, Paths}
import java.time.LocalDate
import java.time.format.DateTimeFormatter
import java.util.concurrent.{Executors, TimeUnit}
import java.util.{Base64, Properties}
import javax.swing.{JLabel, JOptionPane, JPanel, JPasswordField, SwingUtilities}
import scala.jdk.CollectionConverters.given
import scala.collection.mutable
import scala.concurrent.{ExecutionContext, Future, Promise}
import scala.concurrent.duration.Duration
import scala.quoted.{Expr, Quotes, Type}
import scala.reflect.ClassTag
import scala.util.{Using, boundary}
import scala.util.boundary.break
import scala.reflect.runtime.universe.{TypeTag, typeOf}
import scala.sys.process.stringSeqToProcess

object Utils {
  private val logger = Logger[Utils.type]

  private var uniqueIdCounter = 0L
  def uniqueId(): Long = synchronized { uniqueIdCounter += 1; uniqueIdCounter }

  private var systemPropertiesLoaded = false
  def loadSystemProperties(): Unit = if (!systemPropertiesLoaded) synchronized {
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

  def isDistinct[A](values: IterableOnce[A]): Boolean = boundary {
    val set = new mutable.HashSet[A]
    for (v <- values) {
      if (set.contains(v))
        boundary.break(false)
      set += v
    }
    true
  }
  
  private lazy val tempDir = {
    val dir = os.temp.dir(prefix = "assessments", deleteOnExit = true)
    println(s"Temp directory: $dir")
    dir
  }
  def getTempDir(implicit enclosing: FileName): Path = os.temp.dir(dir = tempDir, prefix = enclosing.value, deleteOnExit = false).toNIO
  
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

  def getSystemProperty(property: String, description: String): String = {
    loadSystemProperties()
    val prop = System.getProperty(property)
    if (prop == null)
      throw new RuntimeException(s"Please configure $property in java.properties ($description)")
    prop
  }

  def getSystemPropertyPath(property: String, fileDescription: String): Path = {
    val path = getSystemProperty(property, s"path to $fileDescription")
    val path2 = Path.of(path)
    if (!path2.isAbsolute)
      throw new RuntimeException(s"$property in java.properties must refer to an absolute path")
    path2
  }

  def getSystemPropertyClass[T](property: String, classDescription: String)(implicit typeTag: TypeTag[T]): Class[T] = {
    val className = getSystemProperty(property, s"class name of $classDescription")
    val clazz = try
      getClass.getClassLoader.loadClass(className)
    catch
      case e: ClassNotFoundException =>
        throw new RuntimeException(s"$property in java.properties must refer to an existing class (full class name like package.subpackage.MyClass)")
    if (!typeTag.mirror.runtimeClass(typeTag.tpe).isAssignableFrom(clazz))
      throw new RuntimeException(s"$property in java.properties must refer to a class of type $typeTag")
    clazz.asInstanceOf[Class[T]]
  }

  def getSystemPropertyObject[T](property: String, objectDescription: String)(using classTag: ClassTag[T]): T = {
    val objectName = getSystemProperty(property, s"object name of $objectDescription")
    val clazz = try
      getClass.getClassLoader.loadClass(objectName + "$")
    catch
      case e: ClassNotFoundException =>
        throw new RuntimeException(s"$property in java.properties must refer to an existing object (full object name like package.subpackage.MyObject)")

    val moduleField = try
      clazz.getField("MODULE$")
    catch
      case e: NoSuchFieldException =>
        throw new RuntimeException(s"$property in java.properties must refer to a Scala object, not a class")

    val objectInstance = moduleField.get(null)

    if (!classTag.runtimeClass.isAssignableFrom(objectInstance.getClass))
      throw new RuntimeException(s"$property in java.properties must refer to an object of type $classTag")

    objectInstance.asInstanceOf[T]
  }

    /**
   * Macro that raises a compilation error if compilation happens on or after the specified date.
   *
   * @param date Date string in format "yyyy-MM-dd" (e.g., "2025-10-12")
   * @param message Error message to display
   */
  inline def errorAfter(date: String, message: String): Unit =
    ${ errorAfterImpl('date, 'message) }

  private def errorAfterImpl(dateStr: Expr[String], message: Expr[String])(using Quotes): Expr[Unit] = {
    import scala.quoted.*
    import quotes.reflect.*

    // Extract the string values from the expressions
    val dateString = dateStr.valueOrAbort
    val errorMessage = message.valueOrAbort

    try {
      // Parse the target date
      val formatter = DateTimeFormatter.ofPattern("yyyy-MM-dd")
      val targetDate = LocalDate.parse(dateString, formatter)
      val currentDate = LocalDate.now()

      // Check if current date is on or after the target date
      if (!currentDate.isBefore(targetDate)) {
        report.errorAndAbort(
          s"Compilation error triggered: $errorMessage " +
            s"(compiled on $currentDate, error date was $targetDate)"
        )
      }

      // If we're before the target date, compilation succeeds
      '{ () }

    } catch {
      case _: java.time.format.DateTimeParseException =>
        report.errorAndAbort(
          s"Invalid date format: '$dateString'. Expected format: yyyy-MM-dd"
        )
    }
  }

  def askPassword(passwordDescription: String): String = {
    Seq("/usr/lib/seahorse/ssh-askpass", "--", passwordDescription).!!
  }


  def splitExtFilename(filename: String): (String, String) = {
    val dotIndex = filename.lastIndexOf('.')
    if (dotIndex >= 0 && dotIndex < filename.length - 1) {
      val basename = filename.substring(0, dotIndex)
      val extension = filename.substring(dotIndex + 1)
      (basename, extension)
    } else {
      (filename, "") // No extension found
    }
  }

  def checkInterrupt(): Unit = if (Thread.interrupted) throw new InterruptedException()
  case class Timeout(message: String, extraData: Any*)(implicit context: ExceptionContext) extends ExceptionWithContext(message, extraData)
  /** Runs code with a timeout. Far from perfect: the code may continue running in the background
   * because JVM does not support killing threads (just asking them in a friendly way).
   * Tasks will stop fully when they call [[checkInterrupt]] or in certain System operations (like waiting) */
  def runWithTimeout[A](timeout: Duration, label: String, body: => A)(implicit context: ExceptionContext): A = {
    var threadPromise = Promise[Thread]()
    val executor = Executors.newSingleThreadExecutor((r: Runnable) => {
      val thread = new Thread(r)
      threadPromise.success(thread)
      thread.setDaemon(true)
      thread.setName(label)
      thread
    })
    val future = executor.submit(() => body)

    try {
      val result = future.get(timeout.toNanos, TimeUnit.NANOSECONDS)
      result
    } catch {
      case _: java.util.concurrent.TimeoutException =>
        future.cancel(true) // This sends interrupt signal
        try {
          for (threadTry <- threadPromise.future.value;
               thread <- threadTry.toOption)
            thread.setName("INTERRUPTED-" + label)
            thread.setPriority(Thread.MIN_PRIORITY)
        } catch { case _: Throwable => }
        throw Timeout(s"Timeout encountered ($timeout)")
      case e: java.util.concurrent.ExecutionException =>
        throw e.getCause
    } finally {
      executor.shutdownNow()
    }
  }

  def htmlToPdf(htmlFile: Path, pdfOutputFile: Path): Unit = {
    val command = Seq(
      "chromium",
      "--headless",
      "--disable-gpu",
      "--no-sandbox",
      "--disable-dev-shm-usage",
      "--run-all-compositor-stages-before-draw",
      "--disable-background-timer-throttling",
      "--disable-backgrounding-occluded-windows",
      "--disable-renderer-backgrounding",
      s"--virtual-time-budget=5000", // Wait 5 seconds for MathJax
      "--print-to-pdf-no-header",
      s"--print-to-pdf=${pdfOutputFile.toAbsolutePath}",
      s"file://${htmlFile.toAbsolutePath}"
    )
    command.!!
  }
  
  def findCollision[T, U](items: IterableOnce[T], function: T => U,
                          callback: (T,T) => Unit = { (x:T,y:T) => () }): Option[(T,T)] = boundary[Option[(T,T)]] {
    val map = mutable.HashMap[U,T]()
    for (item <- items) {
      map.put(function(item), item) match {
        case Some(previous) =>
          callback(previous, item)
          break(Some((previous, item)))
        case None =>
      }
    }
    None
  }
}

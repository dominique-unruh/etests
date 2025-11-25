package assessments

import scala.language.implicitConversions
import assessments.Comment.Kind
import assessments.ExceptionContext.initialExceptionContext
import assessments.GradingContext.comments
import externalsystems.Spreadsheet.Index
import externalsystems.{Dynexite, RWTHOnlineGrades, Sciebo, Spreadsheet}
import org.apache.commons.lang3.exception.ExceptionUtils
import org.apache.commons.text.StringEscapeUtils
import org.apache.commons.text.StringEscapeUtils.escapeHtml4
import utils.{IndentedInterpolator, Utils}

import java.io.PrintWriter
import java.nio.file.{Files, Path}
import javax.swing.JOptionPane
import scala.collection.mutable
import scala.util.Using

abstract class Task extends App {
  Utils.loadSystemProperties()
/*  lazy val optionValues = {
    val module = getClass.getField("MODULE$")

    for (method <- getClass.getDeclaredMethods;
         if method.getParameterCount == 0;
         if classOf[Task.Option[?]].isAssignableFrom(method.getReturnType)) yield {
      val option = method.invoke(this).asInstanceOf[Task.Option[?]]
//      println(field.get(this))
      println(option)
    }
  }*/
}

object Task {
  abstract class OptionType[T] {
    def stringToValue(string: String): T

    def valueToString(value: T): String = value.toString

    def popupMessage(name: String, label: String): String =
      s"Please enter $name. (You can also set it in java.properties as $label=...)"

    def askViaPopup(name: String, label: String): T = {
      val message = popupMessage(name, label)
      val response = JOptionPane.showInputDialog(message)
      stringToValue(response)
    }
  }

  implicit object OptionTypeString extends OptionType[String] {
    override def stringToValue(string: String): String = string
  }
  
  implicit object OptionTypeClass extends OptionType[Class[?]] {
    override def stringToValue(string: String): Class[?] =
      getClass.getClassLoader.loadClass(string)

    override def valueToString(value: Class[?]): String =
      value.getCanonicalName
  }

  case class Option[T: OptionType](name: String, label: String) {
    lazy val value: T = {
      val optionType = summon[OptionType[T]]
      val prop = System.getProperty(label)
      if (prop == null)
        optionType.askViaPopup(name, label)
      else
        optionType.stringToValue(prop)
    }
  }
}


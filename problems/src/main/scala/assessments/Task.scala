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
import scala.collection.mutable
import scala.util.Using

abstract class Task extends App {
  Utils.loadSystemProperties()
}




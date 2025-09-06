package tmp

import assessments.DynexiteDefaults.elementName
import assessments.pageelements.InputElement
import assessments.stack.{StackMath, StackParser, SympyExpr}
import assessments.stack.StackMath.Ops
import assessments.stack.StackParser.{maximaToStackMath, parseArray}
import assessments.{DynexiteDefaults, ElementName, Exam, Html, MathContext, SyntaxError}
import externalsystems.MoodleStack
import externalsystems.MoodleStack.{Question, Quiz, inputElementToMoodle, moodleAllowWords, moodleInputType, moodleQuestionVariables}
import me.shadaj.scalapy.py
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}
import utils.{Docker, Python}

import java.io.StringReader
import java.nio.file.Path
import scala.collection.mutable.ArrayBuffer
import scala.reflect.{ClassTag, TypeTest, Typeable}
import scala.reflect.ClassTag
import scala.compiletime.{constValue, erasedValue}
import scala.deriving.Mirror
import scala.reflect.api.TypeTags
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.TypeTag.given
import scala.reflect.runtime.universe.given
import scala.reflect.api.TypeTags
import scala.util.Random

class SimpleTest

object Tmp {
  // Method 3: Simple macro (most flexible, replaces TypeTag)

  import scala.quoted.*


  def main(args: Array[String]): Unit = {
//    val x: py.Dynamic = py.eval("None")
    val x: py.Dynamic = py.eval("[1,2,3]")
//    println(x)
//    println(x == py.None)
    val res = x match
      case Python.none => None
      case Python.List(psi,phi) => Some((SympyExpr(psi), SympyExpr(phi)))
    println(res)
  }
}

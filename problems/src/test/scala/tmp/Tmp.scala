package tmp

import assessments.DynexiteDefaults.{InputElementMethods, elementName, math}
import assessments.pageelements.InputElement
import assessments.stack.{StackMath, StackParser, SympyExpr}
import assessments.stack.StackMath.Ops
import assessments.stack.StackParser.{maximaToStackMath, parseArray}
import assessments.{DynexiteDefaults, ElementName, Exam, Html, MathContext, SyntaxError}
import externalsystems.MoodleStack
import externalsystems.MoodleStack.InputType.matrix
import externalsystems.MoodleStack.{Question, Quiz, inputElementToMoodle, moodleAllowWords, moodleInputType, moodleQuestionVariables}
import me.shadaj.scalapy.py
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}
import utils.Tag.Tags
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

  class Context

  def main(args: Array[String]): Unit = {
    val name = "ans2_sub_2_0"
    println(!name.matches(".*_sub_[0-9]+_[0-9]+"))
    
    val input = DynexiteDefaults.input("transpose(matrix([0,0,i,0]))", Tags(moodleInputType := matrix))
    val str = "transpose(matrix([0,0,i,0]))"
    val parsed = str.math(input)
    println(parsed)
  }
}

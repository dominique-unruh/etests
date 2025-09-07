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

  class Context

  def main(args: Array[String]): Unit = {
    class Player {
      private var _points: Int = 0

      def points(using ctx: Context): Int = _points

      def points_=(value: Int)(using ctx: Context): Unit = {
        _points = math.max(0, value) // Ensure points never go negative
      }

      def points_+=(value: Int)(using ctx: Context): Unit = {
        _points = math.max(0, value) // Ensure points never go negative
      }
    }

    given Context()
    val player = new Player
    player.points = 100 // Works
    player.points = player.points + 50
//    player.points += 50 // Works - becomes player.points = player.points + 50
//    player.points -= 25 // Works
//    player.points *= 2 // Works
    println(player.points) // 250
  }
}

package tmp

import assessments.stack.{StackMath, StackParser}
import assessments.stack.StackMath.Ops
import assessments.{Exam, MathContext, SyntaxError}
import exam.y2025.iqc1.Globals
import ujson.{Arr, Bool, Null, Num, Obj, Str, Value}
import utils.Docker

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
    given MathContext = Globals.mathContext

    val term = StackParser.parse("1/sqrt(2)*-3/2+1/sqrt(2)*1/2")
    val sympy = term.toSympyMC()
    println(sympy.latex)
  }
}

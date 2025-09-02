package tmp

import assessments.stack.{StackMath, StackParser}
import assessments.stack.StackMath.Ops
import assessments.stack.StackParser.{maximaToStackMath, parseArray}
import assessments.{Exam, MathContext, SyntaxError}
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
    val expression = "x\u00f72"
    println(expression)
    val result = Docker.runInDocker(Path.of("docker/stack-parser"),
      Seq("bash", "/parse.sh"),
      files=Map("expression.txt" -> expression), requestedOutputs = Seq("result.txt"))
    val pseudoLatex = result.fileString("result.txt").get
    assert(pseudoLatex.startsWith("\\[ "))
    assert(pseudoLatex.endsWith(" \\]"))
    val json = pseudoLatex.stripPrefix("\\[ ").stripSuffix(" \\]")
    val array = ujson.read(json)
    val maximaTerm = parseArray(array)
    val math = maximaToStackMath(maximaTerm)

    println(math)
  }
}

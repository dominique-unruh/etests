package tmp

import assessments.Exam

import scala.reflect.{ClassTag, TypeTest, Typeable}
import scala.reflect.ClassTag
import scala.compiletime.{constValue, erasedValue}
import scala.deriving.Mirror
import scala.reflect.api.TypeTags
import scala.reflect.runtime.universe.TypeTag
import scala.reflect.runtime.universe.TypeTag.given
import scala.reflect.runtime.universe.given
import scala.reflect.api.TypeTags

class SimpleTest

object Tmp {
  // Method 3: Simple macro (most flexible, replaces TypeTag)

  import scala.quoted.*



  def main(args: Array[String]): Unit = {
    given TypeTag[Int] = TypeTag.Int
    println(summon[TypeTag[Int]])

  }
}

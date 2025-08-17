package tmp

import assessments.{Comment, Commenter, Points}

import scala.util.boundary
import scala.util.boundary.{Label, break}

object Tmp {
  def main(args: Array[String]): Unit = {
    def g(f: Label[Points] ?=> Nothing): Unit = println(boundary(f))
    g {
      if (true)
        break(5:Points)
      else
        break(6:Points)
    }
  }
}

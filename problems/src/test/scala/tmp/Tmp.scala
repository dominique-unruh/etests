package tmp

import externalsystems.Dynexite
import externalsystems.Dynexite.resultsByLearner
import org.commonmark.node.Node
import org.commonmark.parser.*
import org.commonmark.renderer.html.HtmlRenderer
import tmp.Tmp.getClass
import utils.Utils
import utils.Utils.loadSystemProperties

object Tmp {
  def main(args: Array[String]): Unit = {
    loadSystemProperties()
    val learner = "***REMOVED***"
    val attempt = resultsByLearner(learner).get
    val itemId = "d2914rbadbec73f2974g"
//    val attemptId = "d2e468b14kds73bh13c0"
    val attemptId = attempt.attemptId
    val item = attempt.items(14)
    println(s"learner: $learner; item: ${item.name}")
//    val itemId = item.itemId
    println(itemId)
    println("d2914rbadbec73f2974g")
//    println(attemptId)
//    println("d2e468bd2914rbadbec73f2974g14kds73bh13c0")
//    val blueprintId = "d27o253adbec73a2lnp0"
    val blueprintId = Dynexite.theResults.blueprint.blueprintId
//    println(blueprintId)
//    println(Dynexite.theResults.blueprint.blueprintId)
    val url = s"https://dynexite.rwth-aachen.de/t/companies/cpsippjadbec73a3unm0/courses/d27o253adbec73a2lnp0/exams/$blueprintId/grader?item=$itemId&attempt=$attemptId&seed="
    println(url)
  }
}

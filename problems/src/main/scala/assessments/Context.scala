package assessments

import assessments.Context.contexts

import java.util.concurrent.ConcurrentHashMap
import scala.util.Random
import scala.util.Using.Releasable

final case class Context(name: ElementName) {
  private val randomId: Long = Random.nextLong(Long.MaxValue)
  contexts.put(randomId, this)
  def getterCode = s"assessments.Context.getContextById(${randomId}L)"
}

object Context {
  private val contexts = new ConcurrentHashMap[Long, Context]()
  given Releasable[Context] = (context: Context) => contexts.remove(context.randomId)
  def getContextById(id: Long): Context = contexts.get(id)
}
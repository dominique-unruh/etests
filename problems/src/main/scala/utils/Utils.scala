package utils

import scala.collection.mutable

object Utils {
  def uniqueMap[K,V](elems: (K,V)*): Map[K, V] =
    val set = new mutable.HashSet[K]
    for (k <- elems.map(_._1))
      assert(!set.contains(k))
      set += k
    Map(elems*)
}

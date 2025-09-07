package utils

import utils.Tag.Tagged

/** A tag for key/value stores with the intent that the value associated with this tag will be 
 * of type `Value`.
 * 
 * @tparam Owner the type of the intended "owner" of the tag, see [[utils.Tag.Tags]].
 * @tparam Value the type of the intended associated value
 * */
class Tag[Owner, Value](explicitName: String = "", val default: Value)(implicit sourceCodeName: sourcecode.Name) {
  val name: String = if (explicitName.nonEmpty) then explicitName else sourceCodeName.value
  def :=(value: Value) : Tagged[Owner, Value] = Tagged(this, value)
}

object Tag {
  /** A collection of tag/value pairs.
   * Each `(tag,value)` has a tag of type `Tag[O,V]` with `value : V`.
   * 
   * @tparam Owner intended "owner" of this tag collection. This will usually be the type of
   *               the class that contains the tag collection. This ensures that the tag collection
   *               will not contain tags that this class does not understand. All tags in this tag collection
   *               will be of type `Tag[O,V]` with `O >: Owner`.
   **/
  class Tags[-Owner] private (/** Contains `Tag[O,V] -> V` pairs, with `O >: Owner` */
                               private val map: Map[Tag[?, ?], Any]) extends AnyVal {
    def get[Value](tag: Tag[?, Value]): Option[Value] =
      map.get(tag).asInstanceOf[Option[Value]]
    def getOrElse[Value](tag: Tag[?, Value], default: => Value): Value =
      get(tag).getOrElse(default)
    def apply[Value](tag: Tag[?, Value]): Value =
      get(tag).getOrElse(tag.default)
    def +[O, Value](tagged: Tagged[O, Value]): Tags[Owner & O] = {
      assert(!map.contains(tagged.tag))
      new Tags(map + (tagged.tag.asInstanceOf[Tag[?, ?]] -> tagged.value))
    }
  }
  /** A tag/value pair with matching types. */
  case class Tagged[Owner, Value](tag: Tag[Owner, Value], val value: Value)

  object Tagged {
    given [Owner, Value]: Conversion[Tagged[Owner, Value], Tags[Owner]] = Tags(_)
  }

  object Tags {
    def apply[Owner](tags: Tagged[? >: Owner, ?]*): Tags[Owner] = {
      val builder = mkBuilder[Owner]
      for (tagged <- tags)
        builder += tagged
      builder.result()
    }

    def empty[Owner] = new Tags[Owner](Map.empty)

    def mkBuilder[Owner] = new Builder[Owner]()

    class Builder[Owner] private[Tag] () {
      private val builder = Map.newBuilder[Tag[?, ?], Any]

      def +=[O >: Owner, Value](tagged: Tagged[O, Value]) : Unit =
        this += (tagged.tag, tagged.value)

      def +=[O >: Owner, Value](tagValue: (Tag[O, Value], Value)) : Unit =
        builder += tagValue

      def result(): Tags[Owner] = new Tags(builder.result())
    }
  }

  given [Owner]: Conversion[Tag[Owner,Boolean], Tagged[Owner,Boolean]] =
    tag => tag := true
  extension [Owner, A] (tag: Tag[Owner, Seq[A]])
    def :=(value: A): Tagged[Owner, Seq[A]] = tag := Seq(value)
}

package assessments

class ImplicitName[T,N](val name: T) {

}

object ImplicitName {
  given [T : FromString, N <: String & Singleton](using name: sourcecode.Name, n: ValueOf[N]): ImplicitName[T,N] = {
    val explicitName: String = n.value
    if (explicitName == null)
      ImplicitName(summon[FromString[T]].fromString(name.value))
    else
      ImplicitName(summon[FromString[T]].fromString(explicitName))
  }

  trait FromString[T] {
    def fromString(name: String): T
  }
}

package assessments

class ImplicitName[T,N](val name: T) {

}

object ImplicitName {
  given [T : FromString, N <: String & Singleton](using name: sourcecode.Name, n: ValueOf[N]): ImplicitName[T,N] = {
    val explicitName: String = n.value
//    println(s"NAME: [$name], n: [${explicitName}] [${explicitName==null}]")
    if (explicitName == null || explicitName == "")
      ImplicitName(summon[FromString[T]].fromString(name.value))
    else
      ImplicitName(summon[FromString[T]].fromString(explicitName))
  }

  trait FromString[T] {
    def fromString(name: String): T
  }

  given FromString[String] = identity
}

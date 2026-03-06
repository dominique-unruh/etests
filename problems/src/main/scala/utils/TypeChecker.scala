package utils

import scala.reflect.{ClassTag, Typeable}

trait TypeChecker[A] extends Typeable[A] {
  override def toString: String = s"Type: $name"
  val name: String
  /** Tests whether `x` is of type `T`.
   * In contrast to `x.isInstanceOf[T]`, this also checks whether the type parameters are correct (as far as this is
   * still determinable by recursive inspection of `x`).
   *
   * Note: Semantics of types are w.r.t. to the interpretation that Null is separate from AnyRef
   * (like with the [explicit nulls](https://docs.scala-lang.org/scala3/reference/experimental/explicit-nulls.html)
   * option). So to match, e.g., a string that can be null, use a TypeChecker[String|Null].
   **/
  def isInstance(x: Any): Boolean
  final def unapply(x: Any): Option[A & x.type] =
    if (isInstance(x))
      Some(x.asInstanceOf[A & x.type])
    else
      None
  final def |[B](bChecker: TypeChecker[B]): TypeChecker[A | B] = TypeChecker.union(this, bChecker)
  final def &[B](bChecker: TypeChecker[B]): TypeChecker[A & B] = TypeChecker.intersection(this, bChecker)
}

object TypeChecker {
  final class BasicTypeChecker[A](using clazz: ClassTag[A]) extends TypeChecker[A] {
    require(clazz.runtimeClass.getTypeParameters.isEmpty,
      s"${clazz.runtimeClass.getSimpleName} must not have type parameters")
    override val name: String = clazz.runtimeClass.getSimpleName
    override def isInstance(x: Any): Boolean = {
      // clazz.runtimeClass.isInstance(x) would seem to be the same and faster but:
      // e.g., summon[ClassTag[Boolean]].runtimeClass.isInstance(true) == false
      // (due to differences between boxes and primitive types)
      clazz.unapply(x).nonEmpty
    }
  }

  final class UnionTypeChecker[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]) extends TypeChecker[A | B] {
    override val name: String = s"${aChecker.name} | ${bChecker.name}"
    override def isInstance(x: Any): Boolean = aChecker.isInstance(x) || bChecker.isInstance(x)
  }

  final class IntersectionTypeChecker[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]) extends TypeChecker[A & B] {
    override val name: String = s"${aChecker.name} & ${bChecker.name}"
    override def isInstance(x: Any): Boolean = aChecker.isInstance(x) && bChecker.isInstance(x)
  }

  final class IterableTypeChecker[A, C <: Iterable[A]](aChecker: TypeChecker[A], classTag: ClassTag[C]) extends TypeChecker[C] {
    override val name: String = s"${classTag.runtimeClass.getSimpleName}[${aChecker.name}]"
    override def isInstance(x: Any): Boolean = {
      classTag.runtimeClass.isInstance(x) &&
        x.asInstanceOf[Iterable[A]].forall(aChecker.isInstance)
    }
  }

  object AnyTypeChecker extends TypeChecker[Any] {
    override val name: String = "Any"
    override def isInstance(x: Any): Boolean = true
  }

  object NothingTypeChecker extends TypeChecker[Nothing] {
    override val name: String = "Nothing"
    override def isInstance(x: Any): Boolean = true
  }

  /** Private because it's unsafe (easy to construct a wrong type checker using the wrong arguments) */
  private final class LiteralTypeChecker[T](value: T, predicate: Any => Boolean) extends TypeChecker[? <: T & Singleton] {
    override val name: String = s"$value.type"
    override def isInstance(x: Any): Boolean = predicate(x)
  }

  object NullTypeChecker extends TypeChecker[Null] {
    override val name: String = "null"
    override def isInstance(x: Any): Boolean = x match {
      case null => true
      case _ => false
    }
  }

  class PairTypeChecker[A,B](typeCheckerA: TypeChecker[A], typeCheckerB: TypeChecker[B])
    extends TypeChecker[(A,B)] {
    override val name: String = s"(${typeCheckerA.name}, ${typeCheckerB.name})"
    override def isInstance(x: Any): Boolean = x match {
      case (a: Any, b: Any) => typeCheckerA.isInstance(a) && typeCheckerB.isInstance(b)
      case _ => false
    }
  }

  def literal[T <: Singleton](value: T): TypeChecker[value.type] = {
    // We need this helper function because the match gives a compile error
    // if value : T  (supposedly for singleton types, the cases are unreachable code)
    def mkLiteral[U](value: U): TypeChecker[value.type] = value match {
      case valueRef: AnyRef =>
        LiteralTypeChecker(value, { case x: AnyRef => x eq valueRef; case _ => false })
          .asInstanceOf[TypeChecker[value.type]]
      case valueVal: AnyVal =>
        LiteralTypeChecker(value, x => valueVal == x)
          .asInstanceOf[TypeChecker[value.type]]
      case null =>
        NullTypeChecker.asInstanceOf[TypeChecker[value.type]]
    }
    mkLiteral(value)
  }


  def union[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]): TypeChecker[A | B] =
    UnionTypeChecker(aChecker, bChecker)

  def intersection[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]): TypeChecker[A & B] =
    IntersectionTypeChecker(aChecker, bChecker)

  def basic[A](using clazz: ClassTag[A]): TypeChecker[A] = new BasicTypeChecker[A]

  def typeChecker[A](using typeChecker: TypeChecker[A]): TypeChecker[A] = typeChecker

  def pair[A, B](typeCheckerA: TypeChecker[A], typeCheckerB: TypeChecker[B]): TypeChecker[(A, B)] =
    PairTypeChecker(typeCheckerA, typeCheckerB)

  given literalImplicit[T <: Singleton](using value: ValueOf[T]): TypeChecker[T] =
    literal(valueOf[T]).asInstanceOf[TypeChecker[T]]
  given orNull[A](using aChecker: TypeChecker[A]) : TypeChecker[A | Null] =
    union(aChecker, NullTypeChecker)
  given any: TypeChecker[Any] = AnyTypeChecker
  given nul: TypeChecker[Null] = NullTypeChecker
  given nothing: TypeChecker[Nothing] = NothingTypeChecker
  given long: TypeChecker[Long] = basic
  given int: TypeChecker[Int] = basic
  given string: TypeChecker[String] = basic
  given boolean: TypeChecker[Boolean] = basic
  given bigInt: TypeChecker[BigInt] = basic
  given pairImplicit[A,B](using typeCheckerA: TypeChecker[A], typeCheckerB: TypeChecker[B]): TypeChecker[(A,B)] =
    PairTypeChecker(typeCheckerA, typeCheckerB)
}
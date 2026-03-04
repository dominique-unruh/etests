package utils

import scala.reflect.ClassTag

trait TypeChecker[A] {
  override def toString: String = s"Type: $name"
  val name: String
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
  class BasicTypeChecker[A](using clazz: ClassTag[A]) extends TypeChecker[A] {
    require(clazz.runtimeClass.getTypeParameters.isEmpty,
      s"${clazz.runtimeClass.getSimpleName} must not have type parameters")
    override val name: String = clazz.runtimeClass.getSimpleName
    override def isInstance(x: Any): Boolean = clazz.runtimeClass.isInstance(x)
  }

  class UnionTypeChecker[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]) extends TypeChecker[A | B] {
    override val name: String = s"${aChecker.name} | ${bChecker.name}"
    override def isInstance(x: Any): Boolean = aChecker.isInstance(x) || bChecker.isInstance(x)
  }

  class IntersectionTypeChecker[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]) extends TypeChecker[A & B] {
    override val name: String = s"${aChecker.name} & ${bChecker.name}"
    override def isInstance(x: Any): Boolean = aChecker.isInstance(x) && bChecker.isInstance(x)
  }

  class IterableTypeChecker[A, C <: Iterable[A]](aChecker: TypeChecker[A], classTag: ClassTag[C]) extends TypeChecker[C] {
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

  def union[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]): UnionTypeChecker[A, B] =
    UnionTypeChecker(aChecker, bChecker)

  def intersection[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]): IntersectionTypeChecker[A, B] =
    IntersectionTypeChecker(aChecker, bChecker)

  def basic[A](using clazz: ClassTag[A]): BasicTypeChecker[A] = new BasicTypeChecker[A]

  given long: BasicTypeChecker[Long] = basic
  given int: BasicTypeChecker[Int] = basic
  given string: BasicTypeChecker[String] = basic
  given boolean: BasicTypeChecker[Boolean] = basic
  given any: AnyTypeChecker.type = AnyTypeChecker
}
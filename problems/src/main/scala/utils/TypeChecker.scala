package utils

import scala.reflect.ClassTag

trait TypeChecker[A] {
  val name: String
  def unapply(x: Any): Option[A]
}

object TypeChecker {
  class BasicTypeChecker[A](using clazz: ClassTag[A]) extends TypeChecker[A] {
    override val name: String = clazz.runtimeClass.getSimpleName
    override def unapply(x: Any): Option[A] = clazz.unapply(x)
  }

  class UnionTypeChecker[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]) extends TypeChecker[A | B] {
    override val name: String = s"${aChecker.name} | ${bChecker.name}"
    override def unapply(x: Any): Option[A | B] =
      aChecker.unapply(x) match {
        case Some(value) => Some(value)
        case None => bChecker.unapply(x)
      }
  }

  def union[A, B](aChecker: TypeChecker[A], bChecker: TypeChecker[B]): TypeChecker[A | B] =
    UnionTypeChecker(aChecker, bChecker)

  given long: BasicTypeChecker[Long] = new BasicTypeChecker[Long]
  given int: BasicTypeChecker[Int] = new BasicTypeChecker[Int]
  given string: BasicTypeChecker[String] = new BasicTypeChecker[String]
  given boolean: BasicTypeChecker[Boolean] = new BasicTypeChecker[Boolean]
}
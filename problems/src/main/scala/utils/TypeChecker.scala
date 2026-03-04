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

  given union[A, B](using aChecker: TypeChecker[A], bChecker: TypeChecker[B]): TypeChecker[A | B] =
    UnionTypeChecker(aChecker, bChecker)

  given BasicTypeChecker[Long]
  given BasicTypeChecker[Int]
  given BasicTypeChecker[String]
  given BasicTypeChecker[Boolean]
}
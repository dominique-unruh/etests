package utils

final case class UsingWrapper[T, U](value: T) extends AnyVal

object UsingWrapper {
  given [T, U](using value: T): UsingWrapper[T, U] = UsingWrapper(value)
}
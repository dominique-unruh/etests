package utils

import org.checkerframework.checker.units.qual.A

class DefaultValue[A](val witness: A) extends AnyVal

object DefaultValue {
  def witness[A](using defaultValue: DefaultValue[A]): A = defaultValue.witness
  given DefaultValue[Unit] = DefaultValue(())
  given DefaultValue[String] = DefaultValue("")
  given DefaultValue[Int] = DefaultValue(0)

  given pairDefault[A, B](using DefaultValue[A], DefaultValue[B]): DefaultValue[(A, B)] =
    DefaultValue((witness, witness))
}

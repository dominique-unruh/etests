package assessments.pageelements

case class ErrorElement(message: String, files: Map[String, Array[Byte]]) extends StaticElement

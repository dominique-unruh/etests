package externalsystems

import com.github.tototoshi.csv.{CSVFormat, CSVReader, CSVWriter}
import externalsystems.Spreadsheet.{Format, Index, RowNumberIndex, Row, ValidationRule}
import utils.Utils

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.ConcurrentMapHasAsScala
import scala.util.boundary

case class Spreadsheet private (
                            private val header: Seq[String],
                            private val content: Vector[Row],
                            fileFormat: Format,
                            filePath: Option[Path],
                            validationRules: Seq[ValidationRule] = Seq.empty,
                 ) {
  private val rowNumberIndices: ConcurrentHashMap[String, RowNumberIndex] = new ConcurrentHashMap[String, RowNumberIndex]()

  def rowNumberIndex(column: String): RowNumberIndex =
    rowNumberIndices.computeIfAbsent(column, _ =>
      assert(header.contains(column))
      val index = mutable.Map[String, Seq[Int]]()
      for ((row, rowNr) <- content.zipWithIndex) {
        val value = row.cells(column)
        index.get(value) match
          case Some(rowNrs) => index.put(value, rowNrs.appended(rowNr))
          case None => index.put(value, Seq(rowNr))
      }
      RowNumberIndex(index.toMap))

  def valid: Boolean = errors.isEmpty

  /** Contains at least one error if [[valid]]`==false`. */
  lazy val errors: Iterable[String] =
    for (validation <- validationRules.to(LazyList);
         error <- validation.validate(this))
      yield error

  def lookupAll[U](index: Index[U], key: String): Seq[U] =
    val internalIndex = this.rowNumberIndex(index.indexColumn)
    val rowNrs = internalIndex.map.getOrElse(key, Seq.empty)
    rowNrs.map(nr => index.rowMap(nr, content(nr)))

  def lookup[U](index: Index[U], key: String): U =
    lookupAll(index, key) match
      case Seq() => throw new NoSuchElementException(s"key: $key, index: ${index.name}")
      case Seq(value) => value
      case _ => throw new IllegalArgumentException(s"key: $key, index ${index.name}, multiple elements")

  def save(path: Path = filePath.getOrElse(throw IllegalArgumentException("no path saved in spreadsheet, give one explicitly")),
           format: Spreadsheet.Format = fileFormat): Unit = {
    if (errors.nonEmpty) {
      val errors2 = errors.take(10).toSeq
      if (errors2.length == 1)
        throw new RuntimeException("Validation error: " + errors2.head)
      else
        throw new RuntimeException("Validation errors: " + errors2.head.mkString(". "))
    }

    val rawRows = for (row <- content.iterator) yield
      header.map(row.cells)

    format.save(path, Iterator.single(header) ++ rawRows)
  }

  def addValidationRule(validationRule: ValidationRule): Spreadsheet =
    copy(validationRules = validationRules appended validationRule)

  def forgetPath: Spreadsheet = copy(filePath = None)

  def mapRows(f: Row => Row): Spreadsheet = {
    val headerSet = header.toSet
    val newContent = content.map { row =>
      val newRow = f(row)
      if (newRow ne row)
        assert(newRow.cells.keys == headerSet)
      newRow
    }
    copy(content = newContent)
  }
}

object Spreadsheet {
  case class RowNumberIndex(map: Map[String, Seq[Int]])
  case class Row(cells: Map[String, String]) {
    def apply(header: String): String = cells(header)
  }
  case class Index[U](name: String, indexColumn: String, rowMap: (Int, Row) => U)

  def load(path: Path, format: Format): Spreadsheet = {
    val rawRows = format.load(path)
    assert(rawRows.hasNext)
    val header = rawRows.next()
    assert(Utils.isDistinct(header))
    val rows = for (rawRow <- rawRows) yield
      assert(rawRow.length == header.length)
      Row(Map(header.zip(rawRow)*))
    Spreadsheet(header = header, content = Vector.from(rows), fileFormat = format, filePath = Some(path))
  }


  trait ValidationRule {
    /** Should return an empty sequence if validation passes, otherwise one or more error messages. */
    def validate(spreadsheet: Spreadsheet): IterableOnce[String]
  }

  object ValidationRule {
    case class UniqueColumn(column: String) extends ValidationRule {
      override def validate(spreadsheet: Spreadsheet): IterableOnce[String] =
        for ((key, rows) <- spreadsheet.rowNumberIndex(column).map;
             if rows.length >= 2)
          yield s"Same $column occurs on rows ${rows.mkString(", ")}"
    }
  }

  trait Format {
    def load(path: Path): Iterator[Seq[String]]
    def save(path: Path, rows: IterableOnce[Seq[String]]): Unit
  }

  object Format {
    case class CSV(format: CSVFormat, encoding: String) extends Format {
      override def load(path: Path): Iterator[Seq[String]] = {
        val reader = CSVReader.open(path.toFile, encoding)(format)
        reader.iterator
      }

      override def save(path: Path, rows: IterableOnce[Seq[String]]): Unit = {
        val writer = CSVWriter.open(path.toFile, encoding)(format)
        for (row <- rows)
          writer.writeRow(row)
        writer.close()
      }
    }

  }
}



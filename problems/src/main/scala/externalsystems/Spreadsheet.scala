package externalsystems

import com.github.tototoshi.csv.{CSVFormat, CSVReader, CSVWriter, DefaultCSVFormat}
import com.typesafe.scalalogging.Logger
import externalsystems.Spreadsheet.Format.noFormat
import externalsystems.Spreadsheet.{Format, Index, Row, RowNumberIndex, ValidationRule}
import utils.Utils

import java.nio.file.Path
import java.util.concurrent.ConcurrentHashMap
import scala.collection.mutable
import scala.jdk.CollectionConverters.{ConcurrentMapHasAsScala, IteratorHasAsScala, ListHasAsScala}
import scala.util.boundary
import org.odftoolkit.simple.SpreadsheetDocument
import org.odftoolkit.simple.table.Table

import java.io.IOException

/** An immutable in-memory table of string cells with named columns.
  *
  * Backed by a header row plus a sequence of [[Spreadsheet.Row]]s (one map per row keyed by header
  * name). Supports column lookups, validation, and loading/saving in CSV or ODS
  * ([[Spreadsheet.Format]]). All transforming operations return a new [[Spreadsheet]].
  *
  * Construct via the [[Spreadsheet]] companion factory methods ([[Spreadsheet.load]],
  * [[Spreadsheet.fromIterable]], [[Spreadsheet.fromRawRowsIterator]]); the primary constructor is
  * private.
  *
  * @param headers         the column names, in order
  * @param rows            the data rows
  * @param fileFormat      the format used when [[save]] is called without an explicit format
  * @param filePath        the path this spreadsheet was loaded from / saves to by default, if any
  * @param validationRules rules checked by [[valid]] / [[errors]] / [[assertValid]] */
case class Spreadsheet private (
                                 headers: Seq[String],
                                 rows: Vector[Row],
                                 fileFormat: Format = noFormat,
                                 filePath: Option[Path] = None,
                                 validationRules: Seq[ValidationRule] = Seq.empty,
                 ) {
  private val rowNumberIndices: ConcurrentHashMap[String, RowNumberIndex] = new ConcurrentHashMap[String, RowNumberIndex]()

  /** Returns (building and caching on first use) an index mapping each distinct value in `column` to
    * the row numbers where it occurs. `column` must be a header. */
  def rowNumberIndex(column: String): RowNumberIndex =
    rowNumberIndices.computeIfAbsent(column, _ =>
      assert(headers.contains(column))
      val index = mutable.Map[String, Seq[Int]]()
      for ((row, rowNr) <- rows.zipWithIndex) {
        val value = row.cells(column)
        index.get(value) match
          case Some(rowNrs) => index.put(value, rowNrs.appended(rowNr))
          case None => index.put(value, Seq(rowNr))
      }
      RowNumberIndex(index.toMap))

  /** True iff no validation rule reports an error. */
  def valid: Boolean = errors.isEmpty

  /** Contains at least one error if [[valid]]`==false`. */
  lazy val errors: Iterable[String] =
    for (validation <- validationRules.to(LazyList);
         error <- validation.validate(this))
      yield error

  /** Looks up all rows where `index` column equals `key`. Empty if none
    * match.  */
  def lookupAll[U](index: Index[U], key: String): Seq[U] =
    val internalIndex = this.rowNumberIndex(index.indexColumn)
    val rowNrs = internalIndex.map.getOrElse(key, Seq.empty)
    rowNrs.map(nr => index.rowMap(nr, rows(nr)))

  /** Looks up the single row matching `key` via `index`. Throws [[NoSuchElementException]] if none
    * match, [[IllegalArgumentException]] if more than one does. */
  def lookup[U](index: Index[U], key: String): U =
    lookupAll(index, key) match
      case Seq() => throw new NoSuchElementException(s"key: $key, index: ${index.name}")
      case Seq(value) => value
      case _ => throw new IllegalArgumentException(s"key: $key, index ${index.name}, multiple elements")

  /** Throws a [[RuntimeException]] describing up to ten [[errors]] if this spreadsheet is invalid. */
  def assertValid(): Unit =
    if (errors.nonEmpty) {
      val errors2 = errors.take(10).toSeq
      if (errors2.length == 1)
        throw new RuntimeException("Validation error: " + errors2.head)
      else
        throw new RuntimeException("Validation errors: " + errors2.head.mkString(". "))
    }

  /** Writes this spreadsheet (header row plus data) to `path` using `format`. Both default to the
    * path/format the spreadsheet remembers; `save()` throws if no path is known. */
  def save(path: Path = filePath.getOrElse(throw IllegalArgumentException("no path saved in spreadsheet, give one explicitly")),
           format: Spreadsheet.Format = fileFormat): Unit = {
    val rawRows = for (row <- rows.iterator) yield
      headers.map(row.cells)

    format.save(path, Iterator.single(headers) ++ rawRows)
  }

  /** Returns a copy with `validationRule` added to the validation rules. */
  def addValidationRule(validationRule: ValidationRule): Spreadsheet =
    copy(validationRules = validationRules appended validationRule)

  /** Returns a copy that no longer remembers a default [[filePath]]. */
  def forgetPath: Spreadsheet = copy(filePath = None)

  /** Returns a copy with `f` applied to every row. If `f` returns a new row (not the same instance),
    * it must keep exactly the same set of column keys. */
  def mapRows(f: Row => Row): Spreadsheet = {
    val headerSet = headers.toSet
    val newContent = rows.map { row =>
      val newRow = f(row)
      if (newRow ne row)
        assert(newRow.cells.keys == headerSet)
      newRow
    }
    copy(rows = newContent)
  }
}

object Spreadsheet {
  /** Maps each distinct cell value in a column to the row numbers where it occurs. */
  case class RowNumberIndex(map: Map[String, Seq[Int]])

  /** A single row: a map from column name to cell value. */
  case class Row(cells: Map[String, String]) {
    /** The cell value in column `header`. */
    def apply(header: String): String = cells(header)
  }

  /** A named lookup index over a column, mapping a matching `(rowNumber, row)` to a value of type `U`.
    *
    * @param name        human-readable name, used in error messages
    * @param indexColumn the column whose value is the lookup key
    * @param rowMap      builds the result value from a matching row and its row number */
  case class Index[U](name: String, indexColumn: String, rowMap: (Int, Row) => U)

  /** Builds a spreadsheet from raw rows: the first row is taken as the (distinct) header, the rest as
    * data. Every data row must have the same length as the header. */
  def fromRawRowsIterator(rawRows: Iterator[Seq[String]]): Spreadsheet = {
    assert(rawRows.hasNext)
    val header = rawRows.next()
    assert(Utils.isDistinct(header))
    val rows = for (rawRow <- rawRows) yield
      assert(rawRow.length == header.length)
      Row(Map(header.zip(rawRow)*))
    Spreadsheet(headers = header, rows = Vector.from(rows))
  }

  /** Loads a spreadsheet from `path` using `format`, remembering both for later [[Spreadsheet.save]]. */
  def load(path: Path, format: Format): Spreadsheet = {
    val rawRows = format.load(path)
    fromRawRowsIterator(rawRows).copy(fileFormat = format, filePath = Some(path))
  }

  /** Builds a spreadsheet from an explicit header sequence and its data rows. */
  def fromIterable(headers: Seq[String], rows: IterableOnce[Seq[String]]): Spreadsheet = {
    fromRawRowsIterator(Iterator(headers) ++ rows)
  }

  /** A check run over a whole [[Spreadsheet]], yielding error messages for anything wrong. */
  trait ValidationRule {
    /** Should return an empty sequence if validation passes, otherwise one or more error messages. */
    def validate(spreadsheet: Spreadsheet): IterableOnce[String]
  }

  object ValidationRule {
    /** Validation rule requiring every value in `column` to be unique across rows. */
    case class UniqueColumn(column: String) extends ValidationRule {
      override def validate(spreadsheet: Spreadsheet): IterableOnce[String] =
        for ((key, rows) <- spreadsheet.rowNumberIndex(column).map;
             if rows.length >= 2)
          yield s"Same $column occurs on rows ${rows.mkString(", ")}"
    }
  }

  /** A serialization backend: reads/writes raw rows of strings from/to a file. */
  trait Format {
    /** Reads raw rows (including the header row) from `path`. */
    def load(path: Path): Iterator[Seq[String]]
    /** Writes `rows` (including the header row) to `path`. */
    def save(path: Path, rows: IterableOnce[Seq[String]]): Unit
  }

  object Format {
    /** Placeholder format for spreadsheets built in memory; loading/saving throws. */
    case object noFormat extends Format {
      override def load(path: Path): Iterator[Seq[String]] = ???
      override def save(path: Path, rows: IterableOnce[Seq[String]]): Unit = ???
    }

    /** CSV format with a given underlying `CSVFormat` (delimiter etc.) and character encoding. */
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
    object CSV {
      /** UTF-8 CSV using `;` as delimiter and `\n` as line terminator. */
      val default: CSV = CSV(new DefaultCSVFormat {
        override val delimiter: Char = ';'
        override val lineTerminator: String = "\n"
      }, "utf8")
    }

    /** OpenDocument Spreadsheet (`.ods`) format. Select the sheet by exactly one of `sheetName` or
      * `sheetIndex`. Only loading is implemented; saving throws. */
    case class ODS(sheetName: String = null, sheetIndex: Int = -1) extends Format {
      assert(sheetIndex >= 0 || sheetName != null)
      assert(!(sheetIndex != -1 && sheetName != null))

      override def load(path: Path): Iterator[Seq[String]] = {
        val document = SpreadsheetDocument.loadDocument(path.toFile)

        val table: Table = if (sheetName != null)
          Option(document.getTableByName(sheetName)).getOrElse(throw new IOException(s"Sheet '$sheetName' not found"))
        else {
          val tables = document.getTableList.asScala
          if (sheetIndex < 0 || sheetIndex >= tables.size) {
            throw new IndexOutOfBoundsException(s"Sheet index $sheetIndex is out of bounds (0-${tables.size - 1})")
          }
          tables(sheetIndex)
        }

        val columnCount = table.getColumnCount

        for (row <- table.getRowList.iterator.asScala) yield {
          val cells = for (i <- 0 until columnCount) yield
            row.getCellByIndex(i).getStringValue
          cells
        }
      }

      override def save(path: Path, rows: IterableOnce[Seq[String]]): Unit = ???
    }
  }

  private val logger = Logger[Spreadsheet]
}



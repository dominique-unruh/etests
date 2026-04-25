package utils

import com.typesafe.scalalogging.Logger
import org.sqlite.{SQLiteConnection, SQLiteException}

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.sql.{Connection, DriverManager}

/** Persistent cache. To clear it, delete the `.cache` file. */
object PersistentCache {
  private val logger = Logger[this.type]

  private def sha256(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bytes)

  private lazy val connection: Connection = synchronized {
    val cachePath =
      Utils.getSystemPropertyPath("cache.file", "cache file (e.g., /tmp/etests.cache), doesn't have to exist yet")
    logger.debug("Opening SQLite cache")

    val conn = try
      DriverManager.getConnection(s"jdbc:sqlite:$cachePath")
      catch {
        case e: SQLiteException =>
          e.printStackTrace()
          throw RuntimeException(s"Error opening cache $cachePath. Maybe corrupted? Delete that file to fix.", e)
      }
    conn.setAutoCommit(true)

    conn.createStatement().execute(
      """CREATE TABLE IF NOT EXISTS cache (
        |  key_hash BLOB NOT NULL,
        |  key      BLOB NOT NULL,
        |  value    BLOB NOT NULL,
        |  PRIMARY KEY (key_hash, key)
        |)""".stripMargin
    )

    sys.addShutdownHook(cleanup())

    conn
  }

  /** Not required but will make sure DB connection errors are thrown here. */
  def forceInitialization(): Unit = connection

  private lazy val getStatement = connection.prepareStatement(
    "SELECT value FROM cache WHERE key_hash = ? AND key = ?"
  )

  def get(key: Array[Byte]): Option[Array[Byte]] = synchronized {
    getStatement.setBytes(1, sha256(key))
    getStatement.setBytes(2, key)
    val rs = getStatement.executeQuery()
    try if (rs.next()) Some(rs.getBytes(1)) else None
    finally rs.close()
  }

  def getOrCompute[A](key: Array[Byte], toBytes: A => Array[Byte], fromBytes: Array[Byte] => A)(body: => A): A = synchronized {
    get(key) match {
      case Some(cached) => fromBytes(cached)
      case None => 
        val result = body
        put(key, toBytes(result))
        result
    }
  }

  private lazy val putStatement = connection.prepareStatement(
    """INSERT INTO cache (key_hash, key, value) VALUES (?, ?, ?)
      |ON CONFLICT(key_hash, key) DO UPDATE SET value = excluded.value""".stripMargin
  )

  def put(key: Array[Byte], value: Array[Byte]): Unit = synchronized {
    putStatement.setBytes(1, sha256(key))
    putStatement.setBytes(2, key)
    putStatement.setBytes(3, value)
    putStatement.executeUpdate()
  }

  private lazy val deleteStatement = connection.prepareStatement(
    "DELETE FROM cache WHERE key_hash = ? AND key = ?"
  )


  def delete(key: Array[Byte]): Unit = synchronized {
    deleteStatement.setBytes(1, sha256(key))
    deleteStatement.setBytes(2, key)
    deleteStatement.executeUpdate()
  }

  def thinOutCache(): Unit = synchronized {
    val fraction: Int = 1000
    assert(fraction > 0)
    logger.info(s"Removing 1/$fraction of all cache entries")
    val statement = connection.prepareStatement(
      s"""DELETE FROM cache WHERE ABS(random()) % 1000 = 0;""")
    try
      statement.executeUpdate()
    finally
      statement.close()
  }

  private def cleanup(): Unit = {
    thinOutCache()
    connection.close()
  }
}
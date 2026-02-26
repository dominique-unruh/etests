package utils

import com.typesafe.scalalogging.Logger

import java.nio.file.{Files, Path}
import java.security.MessageDigest
import java.sql.{Connection, DriverManager}

/** Persistent cache. To clear it, delete the `.cache` file. */
object Cache {
  private val logger = Logger[this.type]
  private var _conn: Option[Connection] = None

  private def sha256(bytes: Array[Byte]): Array[Byte] =
    MessageDigest.getInstance("SHA-256").digest(bytes)

  private def connection: Connection = synchronized {
    _conn.filter(!_.isClosed).getOrElse {
      logger.debug("Opening SQLite cache")

      val conn = DriverManager.getConnection("jdbc:sqlite:.cache")
      conn.setAutoCommit(true)

      conn.createStatement().execute(
        """CREATE TABLE IF NOT EXISTS cache (
          |  key_hash BLOB NOT NULL,
          |  key      BLOB NOT NULL,
          |  value    BLOB NOT NULL,
          |  PRIMARY KEY (key_hash, key)
          |)""".stripMargin
      )

      _conn = Some(conn)

      sys.addShutdownHook(close())

      conn
    }
  }

  def get(key: Array[Byte]): Option[Array[Byte]] = synchronized {
    val ps = connection.prepareStatement(
      "SELECT value FROM cache WHERE key_hash = ? AND key = ?"
    )
    try {
      ps.setBytes(1, sha256(key))
      ps.setBytes(2, key)
      val rs = ps.executeQuery()
      try if (rs.next()) Some(rs.getBytes(1)) else None
      finally rs.close()
    } finally ps.close()
  }

  def put(key: Array[Byte], value: Array[Byte]): Unit = synchronized {
    val ps = connection.prepareStatement(
      """INSERT INTO cache (key_hash, key, value) VALUES (?, ?, ?)
        |ON CONFLICT(key_hash, key) DO UPDATE SET value = excluded.value""".stripMargin
    )
    try {
      ps.setBytes(1, sha256(key))
      ps.setBytes(2, key)
      ps.setBytes(3, value)
      ps.executeUpdate()
    } finally ps.close()
  }

  def delete(key: Array[Byte]): Unit = synchronized {
    val ps = connection.prepareStatement(
      "DELETE FROM cache WHERE key_hash = ? AND key = ?"
    )
    try {
      ps.setBytes(1, sha256(key))
      ps.setBytes(2, key)
      ps.executeUpdate()
    } finally ps.close()
  }

  def close(): Unit = synchronized {
    _conn match {
      case Some(c) if !c.isClosed =>
        logger.debug("Closing SQLite cache")
        c.close()
        _conn = None
      case _ =>
    }
  }
}
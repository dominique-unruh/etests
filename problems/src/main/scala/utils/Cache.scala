package utils

import com.typesafe.scalalogging.Logger
import org.rocksdb.{Options, RocksDB, RocksDBException}

import java.io.File
import scala.annotation.tailrec

/** Persistent cache. To clear it, delete the `.cache` directory. */
object Cache {
  RocksDB.loadLibrary()

  private val options = new Options().setCreateIfMissing(true)
  private var _cache: Option[RocksDB] = None
  private var lastAccess: Long = 0

  @tailrec
  private def openAvailableCache(count: Int = 1, max: Int = 10): RocksDB = {
    if (count >= max)
      RocksDB.open(options, s".cache/$count")
    else
      try
        RocksDB.open(options, s".cache/$count")
      catch
        case e: RocksDBException if e.getMessage.contains("lock file") =>
          openAvailableCache(count + 1, max)
  }
  
  def cache: RocksDB = synchronized {
    _cache.getOrElse {
      logger.debug("Opening cache")
      val db = openAvailableCache()
      _cache = Some(db)

      // Register shutdown hook to clean up
      sys.addShutdownHook {
        synchronized {
          _cache.foreach(_.close())
          _cache = None
          options.close()
        }
      }

      db
    }
  }



  // Call this when Play reloads (in Global or ApplicationLifecycle)
  def close(): Unit = synchronized {
    _cache match
      case Some(c) =>
        logger.debug("Closing cache")
        c.close()
        _cache = None
      case None =>
  }

  private val logger = Logger[this.type]
}
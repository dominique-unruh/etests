package utils

import com.typesafe.scalalogging.Logger
import org.rocksdb.{Options, RocksDB}

import java.io.File

/** Persistent cache. To clear it, delete the `.cache` directory. */
object Cache {
  RocksDB.loadLibrary()

  private val options = new Options().setCreateIfMissing(true)
  private var _cache: Option[RocksDB] = None
  private var lastAccess: Long = 0

  def cache: RocksDB = synchronized {
    _cache.getOrElse {
      logger.debug("Opening cache")
      val db = RocksDB.open(options, ".cache")
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
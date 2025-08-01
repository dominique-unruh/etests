package externalsystems


import org.rocksdb.{Options, RocksDB}

import java.io.File

object Cache {
  RocksDB.loadLibrary()

  private val options = new Options().setCreateIfMissing(true)
  val cache = RocksDB.open(options, ".cache")
}
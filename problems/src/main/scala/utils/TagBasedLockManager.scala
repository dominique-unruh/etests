package utils

import java.util.concurrent.ConcurrentHashMap
import java.util.concurrent.locks.ReentrantLock
import scala.collection.mutable
import scala.util.{Failure, Success, Try}

class TagBasedLockManager[K] {
  /** Access with this.synchronized! */
  private val locks = new mutable.WeakHashMap[K, ReentrantLock]()

  /**
   * Executes a block of code while holding a lock identified by the given string.
   * If another thread is already holding the lock for the same identifier,
   * this method will block until the lock becomes available.
   *
   * @param lockId The string identifier for the lock
   * @param block The code block to execute while holding the lock
   * @tparam T The return type of the code block
   * @return The result of executing the code block
   */
  def withLock[T](lockId: K)(block: => T): T = {
    val lock = getLockForId(lockId)
    lock.lock()
    try {
      block
    } finally {
      lock.unlock()
    }
  }

  /**
   * Attempts to execute a block of code while holding a lock, but only if
   * the lock can be acquired immediately (non-blocking).
   *
   * @param lockId The string identifier for the lock
   * @param block The code block to execute while holding the lock
   * @tparam T The return type of the code block
   * @return Some(result) if the lock was acquired and block executed, None otherwise
   */
  def tryWithLock[T](lockId: K)(block: => T): Option[T] = {
    val lock = getLockForId(lockId)
    if (lock.tryLock()) {
      try {
        Some(block)
      } finally {
        lock.unlock()
      }
    } else {
      None
    }
  }

  /**
   * Executes a block of code while holding a lock, with exception handling.
   *
   * @param lockId The string identifier for the lock
   * @param block The code block to execute while holding the lock
   * @tparam T The return type of the code block
   * @return Success(result) if execution succeeded, Failure(exception) if it failed
   */
  def withLockSafe[T](lockId: K)(block: => T): Try[T] = {
    val lock = getLockForId(lockId)
    lock.lock()
    try {
      Try(block)
    } finally {
      lock.unlock()
    }
  }

  /**
   * Gets or creates a ReentrantLock for the given identifier.
   * This method is thread-safe and ensures only one lock exists per unique string.
   */
  private def getLockForId(lockId: K): ReentrantLock = synchronized {
    locks.getOrElseUpdate(lockId, new ReentrantLock)
  }

  /**
   * Returns the current number of active locks being managed.
   * Useful for monitoring and debugging.
   */
  def getActiveLockCount: Int = synchronized { locks.size }
}

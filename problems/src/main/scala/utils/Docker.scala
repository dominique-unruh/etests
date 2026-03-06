package utils

import com.typesafe.scalalogging.Logger
import io.circe.generic.auto.*
import io.circe.parser.decode
import io.circe.syntax.*
import utils.Utils

import java.io.IOException
import java.lang.System.currentTimeMillis
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.util.concurrent.{Semaphore, TimeUnit}
import scala.collection.mutable
import scala.concurrent.Future
import scala.sys.process.*
import scala.util.Using
import scala.concurrent.ExecutionContext.Implicits.global

object Docker {
  /** Result of a Docker execution. */
  case class DockerResult(/** Exit code of the Docker command. */
                          exitCode: Int,
                          /** Files produced by the command inside `/workdir`.
                           * Only files requested in `requestedOutputs` in [[runInDocker]] are included. */
                          files: Map[String, Array[Byte]]) {
    /** Returns a file from [[files]], as a string.
     *
     * @param name Name of the file.
     * @param charset Charset to decode the file as.
     * */
    def fileString(name: String, charset: Charset = UTF_8): Option[String] = files.get(name).map(new String(_, charset))
  }

  val buildBound = 4
  /** Only use via withBuildBound! */
  val buildBoundSemaphore = Semaphore(buildBound)

  /** Limit the number of concurrent builds etc. to [[buildBound]]. */
  private def withBuildBound[A](name: String)(body: => A): A = {
    if (!buildBoundSemaphore.tryAcquire(10, TimeUnit.SECONDS)) {
      logger.info(s"Waiting for build semaphore: $name")
      buildBoundSemaphore.acquire()
      logger.info(s"Acquired build semaphore: $name")
    }
    object autoCloseable extends AutoCloseable {
      override def close(): Unit = buildBoundSemaphore.release()
    }
    Using.resource(autoCloseable)(_ => body)
  }

  /** Returns the ID (hash) of an image that is described by an image name (will be pulled) or a path (will be built from Dockerfile) */
  private def getImageId(image: Path | String): String = {
    if (pulledInThisSession.contains(image))
      return pulledInThisSession(image)
    synchronized {
      if (pulledInThisSession.contains(image))
        return pulledInThisSession(image)

      val cacheKey = s"CACHED-DOCKER-IMAGE-ID:${image.getClass}:${image.toString}".getBytes(UTF_8)
      Cache.get(cacheKey) match {
        case Some(cached) =>
          val (time, id) = decode[(Long, String)](String(cached, UTF_8)).toOption.get
          if (time >= currentTimeMillis() - 10 * 60 * 1000) // Rebuild/pull if at least 10 minutes have passed
            return id
        case None =>
      }

      val imageId = image match {
        case image: String =>
          withBuildBound(s"Pulling docker image $image") {
            println(s"Pulling docker image $image")
            Seq("docker", "pull", "--platform=linux/amd64", "--", image).!!
            val images = Seq("docker", "images", "-q", "--", image).!!
            val images2 = images.split('\n')
            logger.debug(s"$image -> ${images2.mkString(", ")}")
            if (images2.length > 1)
              throw RuntimeException(s"runInDocker called with ambiguous image name $image. Maybe you mean $image:latest?")
            images2.head
          }
        case dir: Path =>
          withBuildBound(s"Building docker image $dir") {
            println(s"Building docker image $dir")
            val imageId = Process(command = Seq("docker", "build", "-q", "--platform=linux/amd64", "."), cwd = dir.toFile).!!.trim
            logger.debug(s"$dir -> $imageId")
            imageId
          }
      }
      pulledInThisSession += (image -> imageId)
      Cache.put(cacheKey, (currentTimeMillis(), imageId).asJson.noSpaces.getBytes(UTF_8))

      imageId
      //    logger.debug(s"Using image $imageId")
    }
  }

  private val currentlyRunning = mutable.HashMap[Array[Byte], Future[DockerResult]]()

  /** Runs a command in a docker image.
   *
   * The result is persistently cached.
   * Repeated invocations with the same parameters will use cached results.
   * See [[Cache]] for how to clear the cache.
   *
   * @param image Name of the Docker image, or path to a directory with a Dockerfile
   * @param command Command to execute inside the Docker image
   * @param files Files to provide to the Docker image. They will be mounted in `/workdir` inside the image.
   *              A map from filename to content. If the content is a string, it is UTF-8 encoded.
   * @param requestedOutputs A list of files to return after execution of the Docker image.
   *                         They are expected to be in `/workdir`. It is not an error if those files do not exist.
   * @return Result of the execution. (Exit code and the files from `requestedOutputs`.)
   * */
  def runInDocker(image: String | Path,
                  command: Seq[String],
                  files: Map[String, Array[Byte] | String],
                  requestedOutputs: Seq[String]): Future[DockerResult] = {
    val imageId = getImageId(image)

    //    (new RuntimeException()).printStackTrace() // Useful for tracing where computationally heavy things are executed during object loading.

    val filesBytes = files.view.mapValues({
      case content: String => content.getBytes(UTF_8)
      case content: Array[Byte] => content
    }).toMap

    val argsJson = (imageId, command, filesBytes, requestedOutputs).asJson.noSpacesSortKeys
    val argsJsonBytes = argsJson.getBytes

    synchronized {
      val oldFuture = currentlyRunning.get(argsJsonBytes)
      if (oldFuture.nonEmpty)
        if (oldFuture.get.isCompleted && oldFuture.get.value.get.isFailure)
          currentlyRunning.remove(argsJsonBytes)
        else
          return oldFuture.get

      val newFuture = Future[DockerResult] {
        Cache.get(argsJsonBytes) match
          case None =>
            val result = withBuildBound(s"Running docker image: $image") {
              runInDockerNoCache(imageId = imageId, command = command, files = filesBytes,
                requestedOutputs = requestedOutputs, hashKey = argsJson) }
            Cache.put(argsJsonBytes, result.asJson.noSpaces.getBytes)
            result
          case Some(cached) =>
            decode[DockerResult](new String(cached)).getOrElse(throw IOException("Unparsable cache content"))
      }
      currentlyRunning.put(argsJsonBytes, newFuture)
      newFuture
    }
  }


  private val pulledInThisSession = mutable.Map[String | Path, String]()
  private def runInDockerNoCache(imageId: String,
                                 command: Seq[String],
                                 files: Map[String, Array[Byte]],
                                 requestedOutputs: Seq[String],
                                 hashKey: String): DockerResult = {
    val tempDir = Utils.getTempDir

    for ((file, content) <- files) {
      Files.write(tempDir.resolve(file), content)
    }

    logger.debug(s"Docker inputs: ${files.map((k,v) => k+String(v)).mkString(";")}")

    val dockerCommand = Seq(
      "docker", "run", "--rm",
      "-v", s"$tempDir:/workdir",
      "-w", "/workdir",
      "--platform=linux/amd64",
      imageId) ++ command

    logger.debug(s"Running Docker command: ${dockerCommand.mkString(" ")}")

    val exitCode = dockerCommand.!

    val resultFiles = Map.from(requestedOutputs.flatMap { name =>
      val file = tempDir.resolve(name)
      if (Files.exists(file))
        Some((name, Files.readAllBytes(file)))
      else
        None
    })

    DockerResult(exitCode = exitCode, files = resultFiles)
  }

  private val logger = Logger[Docker.type]
}

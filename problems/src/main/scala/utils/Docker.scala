package utils

import com.typesafe.scalalogging.Logger
import io.circe.generic.semiauto.deriveCodec
import io.circe.{Codec, Decoder, Encoder}
import io.circe.parser.decode
import io.circe.syntax.*
import utils.Utils

import java.io.IOException
import java.lang.System.{currentTimeMillis, out}
import java.nio.charset.StandardCharsets.UTF_8
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import java.time.Instant
import java.util
import java.util.Base64
import java.util.concurrent.{Semaphore, TimeUnit}
import scala.collection.mutable
import scala.concurrent.Future
import scala.sys.process.*
import scala.util.{Try, Using}
import scala.concurrent.ExecutionContext.Implicits.global

object Docker {
  /** Result of a Docker execution. */
  case class DockerResult(/** Exit code of the Docker command. */
                          exitCode: Int,
                          /** Output of the Docker command. (Mixed stdout, stderr) */
                          output: String,
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

  given dockerResultCodec: Codec[DockerResult] = {
    import utils.CirceCodecs.byteArrayCodec
    deriveCodec[DockerResult]
  }

  private case class DockerKey(imageId: String, command: Seq[String], filesBytes: Map[String, Array[Byte]], requestedOutputs: Seq[String])

  private given dockerKeyCodec: Codec[DockerKey] = {
    import utils.CirceCodecs.byteArrayCodec
    deriveCodec[DockerKey]
  }

  /** Optional remote Docker daemon host (`docker.host` in java.properties). When set, all docker
   * invocations get a `-H <host>` global option and concurrency is reduced (see [[buildBound]]). */
  private def dockerHost: Option[String] = Utils.getSystemPropertyOptional("docker.host", "remote Docker daemon host")

  /** The `docker` command prefix, including a `-H <host>` global option when [[dockerHost]] is set.
   * Prepend this to every docker invocation instead of a literal `Seq("docker", ...)`. */
  private def dockerCmd: Seq[String] = dockerHost match {
    case Some(host) => Seq("docker", "-H", host)
    case None => Seq("docker")
  }

  /** Max number of concurrent docker builds/runs. Lowered to 1 for a remote daemon (see [[dockerHost]])
   * since a shared/remote daemon does not parallelize as well. */
  val buildBound = if (dockerHost.isDefined) 1 else 4
  /** Only use via withBuildBound! */
  private val buildBoundSemaphore = Semaphore(buildBound)

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
  private def getImageId(image: Path | String, invalidateCache: Boolean = false): String = {
    if (!invalidateCache && pulledInThisSession.contains(image))
      return pulledInThisSession(image)
    pulledInThisSession.synchronized {
      if (pulledInThisSession.contains(image))
        return pulledInThisSession(image)

      val cacheKey = s"CACHED-DOCKER-IMAGE-ID:${image.getClass}:${image.toString}".getBytes(UTF_8)
      PersistentCache.get(cacheKey) match {
        case Some(cached) if !invalidateCache =>
          val (time, id) = decode[(Long, String)](String(cached, UTF_8)).toOption.get
          if (time >= currentTimeMillis() - 10 * 60 * 1000) // Rebuild/pull if at least 10 minutes have passed
            return id
        case _ =>
      }

      val imageId = image match {
        case image: String =>
          withBuildBound(s"Pulling docker image $image") {
            println(s"Pulling docker image $image")
            (dockerCmd ++ Seq("pull", "--platform=linux/amd64", "--", image)).!!
            val images = (dockerCmd ++ Seq("images", "-q", "--", image)).!!
            val images2 = images.split('\n')
            logger.debug(s"$image -> ${images2.mkString(", ")}")
            if (images2.length > 1)
              throw RuntimeException(s"runInDocker called with ambiguous image name $image. Maybe you mean $image:latest?")
            images2.head
          }
        case dir: Path =>
          withBuildBound(s"Building docker image $dir") {
            println(s"Building docker image $dir")
            val imageId = Process(command = dockerCmd ++ Seq("build", "-q", "--platform=linux/amd64", "."), cwd = dir.toFile).!!.trim
            logger.debug(s"$dir -> $imageId")
            imageId
          }
      }
      pulledInThisSession += (image -> imageId)
      PersistentCache.put(cacheKey, (currentTimeMillis(), imageId).asJson.noSpaces.getBytes(UTF_8))

      imageId
      //    logger.debug(s"Using image $imageId")
    }
  }

//  private val currentlyRunning = mutable.HashMap[ByteKey, (java.time.Instant, Future[DockerResult], String)]()

//  private val garbageCollectionDelay = 60
//  private val garbageCollectionFrequency = 10
//  private var lastGarbageCollection = Instant.now()
//  private def garbageCollection(): Unit = synchronized {
//    if (lastGarbageCollection.isBefore(Instant.now().minusSeconds(garbageCollectionFrequency)))
//      return
//    currentlyRunning.filterInPlace {
//      case (_, (time, _, _)) => time.isAfter(Instant.now().minusSeconds(garbageCollectionDelay))
//    }
//    lastGarbageCollection = Instant.now()
//  }

  /** Runs a command in a docker image.
   *
   * The result is persistently cached.
   * Repeated invocations with the same parameters will use cached results.
   * See [[PersistentCache]] for how to clear the cache.
   *
   * @param image Name of the Docker image, or path to a directory with a Dockerfile
   * @param command Command to execute inside the Docker image.
   *                (Empty sequence in order to run the command configured in the docker image)
   * @param files Files to provide to the Docker image. They will be mounted in `/workdir` inside the image.
   *              A map from filename to content. If the content is a string, it is UTF-8 encoded.
   * @param requestedOutputs A list of files to return after execution of the Docker image.
   *                         They are expected to be in `/workdir`. It is not an error if those files do not exist.
   * @param shortDescription Short description for debug outputs
   * @return Result of the execution. (Exit code and the files from `requestedOutputs`.)
   * */
  def runInDocker(image: String | Path,
                  command: Seq[String] = Seq.empty,
                  files: Map[String, Array[Byte] | String],
                  requestedOutputs: Seq[String],
                  shortDescription: String,
                  invalidateCache: Boolean = false): Future[DockerResult] = {
    val imageId = getImageId(image, invalidateCache = invalidateCache)

    //    (new RuntimeException()).printStackTrace() // Useful for tracing where computationally heavy things are executed during object loading.

    val filesBytes = files.view.mapValues({
      case content: String => content.getBytes(UTF_8)
      case content: Array[Byte] => content
    }).toMap

    val argsJson = DockerKey(imageId, command, filesBytes, requestedOutputs).asJson.noSpacesSortKeys
    val argsJsonBytes = ByteKey(argsJson.getBytes)

//    synchronized {
//      logCurrentlyRunningDockers()
//      logger.debug(s"Need docker for: ${shortDescription}")
//
//      currentlyRunning.get(argsJsonBytes) match {
//        case Some((time, oldFuture, oldDescription)) if !invalidateCache =>
//          if (oldFuture.isCompleted && oldFuture.value.get.isFailure)
//            currentlyRunning.remove(argsJsonBytes)
//          else
//            logger.debug(s"Reusing docker from ${java.time.Duration.between(time, Instant.now()).getSeconds}s ago")
//            currentlyRunning.update(argsJsonBytes, (Instant.now(), oldFuture, oldDescription))
//            return oldFuture
//        case _ =>
//      }

//      val newFuture = Future[DockerResult] {
    FutureCache.evaluate(("DOCKER",argsJsonBytes)) {
      logger.debug(s"Looking for cached docker result for: $shortDescription")
      PersistentCache.get(argsJsonBytes.bytes) match
        case Some(cached) if !invalidateCache =>
          decode[DockerResult](new String(cached)).getOrElse(throw IOException("Unparsable cache content"))
        case _ =>
          val result = withBuildBound(s"Running docker image: $image (for $shortDescription)") {
            runInDockerNoCache(imageId = imageId, command = command, files = filesBytes,
              shortDescription = shortDescription,
              requestedOutputs = requestedOutputs, hashKey = argsJson) }
          logger.debug(s"Finished docker for: $shortDescription")
          PersistentCache.put(argsJsonBytes.bytes, result.asJson.noSpaces.getBytes)
          result
    }

//      currentlyRunning.update(argsJsonBytes, (Instant.now(), newFuture, shortDescription))
//      garbageCollection()
//      newFuture
//    }
  }

/*
  private def logCurrentlyRunningDockers(): Unit = {
    val running = currentlyRunning.toSeq
      .filter { case (_, (_, future, _)) => !future.isCompleted }
    if (running.nonEmpty)
      logger.debug(s"Currently running ${running.size} dockers: ${running
          .map { case (_, (_, _, name)) => name }
          .mkString("; ")
      }")
    else
      logger.debug("Currently no running dockers.")
  }
*/

  private val pulledInThisSession = mutable.Map[String | Path, String]()
  private def runInDockerNoCache(imageId: String,
                                 command: Seq[String],
                                 files: Map[String, Array[Byte]],
                                 shortDescription: String,
                                 requestedOutputs: Seq[String],
                                 hashKey: String): DockerResult = {
    val tempDir = Utils.getTempDir

    for ((file, content) <- files) {
      Files.write(tempDir.resolve(file), content)
    }

//    logger.debug(s"Docker inputs: ${files.map((k,v) => k+String(v)).mkString(";")}")

    // We do not use a bind mount (`-v $tempDir:/workdir`) to provide the input files and collect the outputs.
    // A bind mount references a path on the *Docker daemon's* host, which is not our filesystem when the daemon
    // is remote or shared via docker-outside-of-docker (the mount would silently be empty). Instead we create the
    // container, `docker cp` the inputs in, run it, and `docker cp` the requested outputs back out. This works
    // regardless of where the daemon lives.
    val createCommand = dockerCmd ++ Seq(
      "create",
      "-w", "/workdir",
      "--platform=linux/amd64",
      imageId) ++ command

    logger.debug(s"Creating Docker container (for $shortDescription): ${createCommand.mkString(" ")}")

    val containerId = createCommand.!!.trim

    try {
      // Copy the staged input files into /workdir (the `/.` makes docker copy the directory *contents*, and
      // /workdir is created if it does not exist).
      val cpInExit = (dockerCmd ++ Seq("cp", s"$tempDir/.", s"$containerId:/workdir")).!(ProcessLogger(_ => ()))
      if (cpInExit != 0)
        throw IOException(s"Failed to copy inputs into Docker container (for $shortDescription)")

      val output = StringBuffer() // Not using StringBuilder (not thread safe)
      // `docker start -a` attaches to the container's stdout/stderr and exits with the container's exit code.
      val exitCode = (dockerCmd ++ Seq("start", "-a", containerId)).!(ProcessLogger(line => output.append(line).append('\n')))

      // Copy back the requested outputs. It is not an error if a file does not exist (docker cp then fails and we
      // skip it), matching the documented behaviour of runInDocker.
      val resultFiles = Map.from(requestedOutputs.flatMap { name =>
        val dest = tempDir.resolve(name)
        val cpOutExit = (dockerCmd ++ Seq("cp", s"$containerId:/workdir/$name", dest.toString)).!(ProcessLogger(_ => ()))
        if (cpOutExit == 0 && Files.exists(dest))
          Some((name, Files.readAllBytes(dest)))
        else
          None
      })

      DockerResult(exitCode = exitCode, output = output.toString, files = resultFiles)
    } finally {
      (dockerCmd ++ Seq("rm", "-f", containerId)).!(ProcessLogger(_ => ()))
    }
  }

  private final class ByteKey(val bytes: Array[Byte]) {
    private val cachedHash = util.Arrays.hashCode(bytes)

    override def hashCode(): Int = cachedHash

    override def equals(obj: Any): Boolean = obj match {
      case that: ByteKey =>
        this.cachedHash == that.cachedHash && util.Arrays.equals(this.bytes, that.bytes)
      case _ =>
        false
    }
  }

  private val logger = Logger[Docker.type]
}

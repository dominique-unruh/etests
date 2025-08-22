package utils

import com.typesafe.scalalogging.Logger
import io.circe.generic.auto.*
import io.circe.parser.decode
import io.circe.syntax.*
import utils.Utils

import java.io.IOException
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.{Files, Path}
import scala.collection.mutable
import scala.sys.process.*

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
    def fileString(name: String, charset: Charset = StandardCharsets.UTF_8): Option[String] = files.get(name).map(new String(_, charset))
  }

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
                  requestedOutputs: Seq[String]): DockerResult = {

    if (!pulledInThisSession.contains(image)) { synchronized { image match {
      case image: String =>
        println(s"Pulling docker image $image")
        Seq("docker", "pull", "--platform=linux/amd64", "--", image).!!
        val images = Seq("docker", "images", "-q", "--", image).!!
        val images2 = images.split('\n')
        logger.debug(s"$image -> ${images2.mkString(", ")}")
        if (images2.length > 1)
          throw RuntimeException(s"runInDocker called with ambiguous image name $image. Maybe you mean $image:latest?")
        val imageId = images2.head
        pulledInThisSession += (image -> imageId)
      case dir: Path =>
        println(s"Building docker image $dir")
        val imageId = Process(command = Seq("docker", "build", "-q", "--platform=linux/amd64", "."), cwd = dir.toFile).!!.trim
        logger.debug(s"$dir -> $imageId")
        pulledInThisSession += (image -> imageId)
    } } }

    val imageId = pulledInThisSession(image)
    logger.debug(s"Using image $imageId")

    val filesBytes = files.view.mapValues({
      case content: String => content.getBytes(StandardCharsets.UTF_8)
      case content: Array[Byte] => content
    }).toMap

    val argsJson = (imageId, command, filesBytes, requestedOutputs).asJson.noSpacesSortKeys
    val argsJsonBytes = argsJson.getBytes

    Cache.cache.get(argsJsonBytes) match
      case null =>
        Thread.sleep(5000)
        val result = runInDockerNoCache(imageId=imageId, command = command, files = filesBytes,
          requestedOutputs = requestedOutputs, hashKey = argsJson)
        Cache.cache.put(argsJsonBytes, result.asJson.noSpaces.getBytes)
        result
      case cached =>
        decode[DockerResult](new String(cached)).getOrElse(throw IOException("Unparsable cache content"))
  }

  private val pulledInThisSession = mutable.Map[String | Path, String]()
  private val lockManager = TagBasedLockManager[String]()
  private def runInDockerNoCache(imageId: String,
                                 command: Seq[String],
                                 files: Map[String, Array[Byte]],
                                 requestedOutputs: Seq[String],
                                 hashKey: String): DockerResult = lockManager.withLock(hashKey) {
    val tempDir = Utils.getTempDir

    for ((file, content) <- files) {
      Files.write(tempDir.resolve(file), content)
    }

    val dockerCommand = Seq(
      "docker", "run", "--rm",
      "-v", s"$tempDir:/workdir",
      "-w", "/workdir",
      "--platform=linux/amd64",
      imageId) ++ command

    println(s"Running Docker command: ${dockerCommand.mkString(" ")}")

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

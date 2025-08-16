package utils

import io.circe.generic.auto.*
import io.circe.parser.decode
import io.circe.syntax.*
import utils.Utils

import java.io.IOException
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files
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
   * @param image Name of the Docker image
   * @param command Command to execute inside the Docker image
   * @param files Files to provide to the Docker image. They will be mounted in `/workdir` inside the image.
   *              A map from filename to content. If the content is a string, it is UTF-8 encoded.
   * @param requestedOutputs A list of files to return after execution of the Docker image.
   *                         They are expected to be in `/workdir`. It is not an error if those files do not exist.
   * @return Result of the execution. (Exit code and the files from `requestedOutputs`.)
   * */
  def runInDocker(image: String,
                  command: Seq[String],
                  files: Map[String, Array[Byte] | String],
                  requestedOutputs: Seq[String]): DockerResult = {

    val filesBytes = files.view.mapValues({
      case content: String => content.getBytes(StandardCharsets.UTF_8)
      case content: Array[Byte] => content
    }).toMap

    val argsJson = (image, command, filesBytes, requestedOutputs).asJson.noSpacesSortKeys.getBytes

    Cache.cache.get(argsJson) match
      case null =>
        val result = runInDockerNoCache(image=image, command = command, files = filesBytes, requestedOutputs = requestedOutputs)
        Cache.cache.put(argsJson, result.asJson.noSpaces.getBytes)
        result
      case cached =>
        decode[DockerResult](new String(cached)).getOrElse(throw IOException("Unparsable cache content"))
  }

  private val pulledInThisSession = mutable.Set[String]()
  private def runInDockerNoCache(image: String,
                    command: Seq[String],
                    files: Map[String, Array[Byte]],
                    requestedOutputs: Seq[String]): DockerResult = {
    val tempDir = Utils.getTempDir

    if (!pulledInThisSession.contains(image)) {
      println(s"Pulling docker image $image")
      Seq("docker", "pull", "--", image).!!
      pulledInThisSession += image
    } else {
      println(s"Already pulled docker image $image")
    }

    for ((file, content) <- files) {
      Files.write(tempDir.resolve(file), content)
    }

    val dockerCommand = Seq(
      "docker", "run", "--rm",
      "-v", s"$tempDir:/workdir",
      "-w", "/workdir",
      image) ++ command

    println(s"Running Docker command: ${dockerCommand.mkString(" ")}")

    Seq("ls", "-lh", tempDir.toString).!

    val exitCode = dockerCommand.!

    Seq("ls", "-lh", tempDir.toString).!

    val resultFiles = Map.from(requestedOutputs.flatMap { name =>
      val file = tempDir.resolve(name)
      if (Files.exists(file))
        Some((name, Files.readAllBytes(file)))
      else
        None
    })

    DockerResult(exitCode = exitCode, files = resultFiles)
  }

}

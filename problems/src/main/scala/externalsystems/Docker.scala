package externalsystems

import utils.Utils

import scala.sys.process.*
import io.circe.generic.auto.*
import io.circe.syntax.*
import io.circe.parser.decode

import java.io.IOException
import java.nio.charset.{Charset, StandardCharsets}
import java.nio.file.Files
import scala.collection.mutable

object Docker {
  case class DockerResult(exitCode: Int, files: Map[String, Array[Byte]]) {
    def fileString(name: String, charset: Charset = StandardCharsets.UTF_8): Option[String] = files.get(name).map(new String(_, charset))
  }

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
        println("CACHED!")
        decode[DockerResult](new String(cached)).getOrElse(throw IOException("Unparsable cache content"))
  }

  private val pulledInThisSession = mutable.Set[String]()
  private def runInDockerNoCache(image: String,
                    command: Seq[String],
                    files: Map[String, Array[Byte]],
                    requestedOutputs: Seq[String]): DockerResult = {
    val tempDir = Utils.getTempDir.toNIO

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

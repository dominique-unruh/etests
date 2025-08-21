package utils

import org.scalatest.funsuite.AnyFunSuiteLike

class DockerTest extends AnyFunSuiteLike {
  test ("cp workdir") {
    val result = Docker.runInDocker(image = "archlinux:latest", command = "cp in.txt out.txt".split(' '),
      files=Map("in.txt" -> "hello"), requestedOutputs = Seq("out.txt"))
    assert(result.exitCode == 0)
    assert(result.fileString("out.txt").contains("hello"))
  }
}

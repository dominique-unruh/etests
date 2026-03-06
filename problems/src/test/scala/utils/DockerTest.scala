package utils

import org.scalatest.funsuite.AnyFunSuiteLike
import utils.Utils.awaitResult

class DockerTest extends AnyFunSuiteLike {
  test ("cp workdir") {
    val result = Docker.runInDocker(image = "archlinux:latest", command = "cp in.txt out.txt".split(' '),
      files=Map("in.txt" -> "hello"), requestedOutputs = Seq("out.txt")).awaitResult()
    assert(result.exitCode == 0)
    assert(result.fileString("out.txt").contains("hello"))
  }

  test("Concurrency") {
    val random = Math.random()
    val futures = for (i <- 1 to 10) yield
      Docker.runInDocker(image = "archlinux:latest", command = Seq("sleep", (i+5).toString),
        files = Map("in.txt" -> random.toString), requestedOutputs = Seq.empty)
    for ((future, index) <- futures.zipWithIndex) {
      println(s"Checking result $index")
      val result = future.awaitResult()
      assert(result.exitCode == 0)
    }
  }

}

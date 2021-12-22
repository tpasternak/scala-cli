// using lib "com.lihaoyi::os-lib:0.8.0"
// using lib "com.lihaoyi::pprint:0.7.1"
// using scala "2.13"

// Usage: scala-cli gcbenchmark.scala -- <path_to_scala_cli_executable>

import scala.concurrent.duration._

case class Result(
  env: Map[String, String],
  maxTime: Duration,
  avarageTime: Duration,
  maxMemoryFootprint: Int,
  idleMemoryFootprint: Int
)

object Main {
  val workspace =
    os.temp.dir(os.pwd, "tmp-") // where the temporary files are stored
  val projectSize =
    30 // Number of files in a generated project used in benchmark
  val numberOfBuilds = 5 // How many times run build for each setup
  val idleWait =
    60 // In seconds. Wait after builds are done, to measure how much memory JVM returns to OS

  val parEnv = Map("BLOOP_JAVA_OPTS" -> "-XX:+UseParallelGC -Xmx4G")
  val zgcEnv = Map(
    "BLOOP_JAVA_OPTS" -> "-XX:+UseZGC -XX:SoftMaxHeapSize=2G -XX:ZUncommitDelay=10"
  )
  val setups = Seq(parEnv, zgcEnv)

  def main(args: Array[String]): Unit = {
    val scalaCli = pprint.log(args(0))
    val classes  = (1 to projectSize).map(i => s"Bench$i")
    def scalaFile(objectName: String, n: Int) = s"""
                                                   |object $objectName {
                                                   |def donothing(i:Int) = {}
                                                   |${"  donothing(1+1)\n" * n}
                                                   |}
                                                   |""".stripMargin
    def bloopMemory(bloopPid: Int) = {
      val bloopMemory = os
        .proc("ps", "-o", "rss", bloopPid)
        .call()
        .out
        .text()
        .linesIterator
        .toList(1)
        .toInt / 1024
      bloopMemory
    }

    def build(rand: Int, env: Map[String, String]): Duration = {
      for { c <- classes } os.write.over(
        workspace / s"$c.scala",
        scalaFile(s"$c", 1000 + rand)
      )
      val start = System.nanoTime()
      os.proc(
        scalaCli,
        "compile",
        classes.map(c => c + ".scala")
      ).call(cwd = workspace, env = env, stout = os.Inherit)
      val stop    = System.nanoTime()
      val elapsed = 1.0 * (stop - start) / 1000000000
      elapsed.seconds
    }

    def bloopPid =          "(\\d+) bloop[.]Server".r
          .findFirstMatchIn(processes)
          .get
          .group(1)
          .toInt


    val results = for { env <- setups } yield {
      os.proc("kill", bloopPid)
      println("=" * 80)
      pprint.log(env)
      build(0, env)
      val processes = os.proc("jps", "-l").call().out.text()
      pprint.log(bloopPid)
      val buildResults = (1 to numberOfBuilds).map { i =>
        val elapsed = build(i, env)
        val memory  = bloopMemory(bloopPid)
        pprint.log(f"$memory MB, $elapsed")
        (memory, elapsed)
      }
      pprint.log(buildResults)
      val idleResults = for { i <- 1 to idleWait } yield {
        Thread.sleep(1000)
        val memory = bloopMemory(bloopPid)
        pprint.log(s"$memory MB")
        memory
      }

      pprint.log(idleResults)

      val res = Result(
        env = env,
        maxTime = buildResults.map(_._2).max.toSeconds.seconds,
        avarageTime = (buildResults
          .map(_._2)
          .fold(0.seconds)(_ + _) / buildResults.size).toSeconds.seconds,
        maxMemoryFootprint = buildResults.map(_._1).max,
        idleMemoryFootprint = idleResults.min
      )
      res
    }
    println("=" * 80)
    pprint.log(results)
  }
}

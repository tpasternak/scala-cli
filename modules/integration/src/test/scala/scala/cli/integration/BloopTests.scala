package scala.cli.integration

import com.eed3si9n.expecty.Expecty.expect

import scala.cli.integration.util.BloopUtil

class BloopTests extends munit.FunSuite {

  def runScalaCli(args: String*) = os.proc(TestUtil.cli, args)

  private lazy val bloopDaemonDir =
    BloopUtil.bloopDaemonDir(runScalaCli("directories").call().out.text())

  // temporary, bleep exit does exit, but is having issues later on…
  private def exitCheck = false

  val dummyInputs = TestInputs(
    Seq(
      os.rel / "Test.scala" ->
        """// using scala "2.13"
          |object Test {
          |  def main(args: Array[String]): Unit =
          |    println("Hello " + "from test")
          |}
          |""".stripMargin
    )
  )

  def testScalaTermination(
    currentBloopVersion: String,
    shouldRestart: Boolean
  ): Unit = TestUtil.retryOnCi() {
    dummyInputs.fromRoot { root =>

      val bloop = BloopUtil.bloop(currentBloopVersion, bloopDaemonDir)
      bloop("exit").call(cwd = root, stdout = os.Inherit, check = exitCheck)
      bloop("about").call(cwd = root, stdout = os.Inherit)

      val output = os.proc(TestUtil.cli, "run", ".")
        .call(cwd = root, stderr = os.Pipe, mergeErrIntoOut = true)
        .out.text()
      expect(output.contains("Hello from test"))
      if (shouldRestart)
        output.contains("Shutting down unsupported Bloop")
      else
        output.contains("No need to restart Bloop")

      val versionLine = bloop("about").call(cwd = root).out.lines()(0)
      expect(versionLine == "bloop v" + Constants.bloopVersion)
    }
  }

  // Disabled until we have at least 2 Bleep releases
  // test("scala-cli terminates incompatible bloop") {
  //   testScalaTermination("1.4.8-122-794af022", shouldRestart = true)
  // }

  test("scala-cli keeps compatible bloop running") {
    testScalaTermination(Constants.bloopVersion, shouldRestart = false)
  }

  test("invalid bloop options passed via cli cause bloop start failure") {
    TestInputs(Seq()).fromRoot { root =>
      runScalaCli("bloop", "exit").call(cwd = root, check = exitCheck)
      val res = runScalaCli("bloop", "start", "--bloop-java-opt", "-zzefhjzl").call(
        cwd = root,
        stderr = os.Pipe,
        check = false,
        mergeErrIntoOut = true
      )
      expect(res.exitCode == 1)
      expect(res.out.text().contains("Server didn't start"))
    }
  }

  test("invalid bloop options passed via global bloop config json file cause bloop start failure") {
    val inputs = TestInputs(
      Seq(
        os.rel / "bloop.json" ->
          """|{
             | "javaOptions" : ["-Xmx1k"]
             | }""".stripMargin
      )
    )

    inputs.fromRoot { root =>
      runScalaCli("bloop", "exit").call(check = exitCheck)
      val res = runScalaCli(
        "bloop",
        "start",
        "--bloop-global-options-file",
        (root / "bloop.json").toString()
      ).call(cwd = root, stderr = os.Pipe, check = false)
      expect(res.exitCode == 1)
      expect(res.err.text().contains("Server didn't start") || res.err.text().contains(
        "java.lang.OutOfMemoryError: Garbage-collected heap size exceeded"
      ))
    }
  }

}

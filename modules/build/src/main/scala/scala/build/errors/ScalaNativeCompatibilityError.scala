package scala.build.errors

case object ScalaNativeCompatibilityError
    extends BuildException(
      """scala-cli: invalid option: '--native' for scripts is supported only for scala 2.13.*
        |Please try one of the following combinations:
        |  scala-cli --native -S 2.12 <...> (for *.sc files)
        |  scala-cli --native -S 2.13 <...> (for *.sc & *.scala files)
        |""".stripMargin
    )

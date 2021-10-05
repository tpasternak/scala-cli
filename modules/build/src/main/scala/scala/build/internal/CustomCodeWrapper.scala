package scala.build.internal

case object CustomCodeWrapper extends CodeWrapper {
  def mainClassObject(className: Name): Name = {
    Name(className.raw ++ "_sc")
  }

  private val userCodeNestingLevel = 1
  def apply(
    code: String,
    pkgName: Seq[Name],
    indexedWrapperName: Name,
    extraCode: String
  ) = {
    val name      = mainClassObject(indexedWrapperName).backticked
    val traitName = Name(s"${indexedWrapperName.raw}_trait").backticked
    val mainObjectCode = s"""|
                             |object ${name} {
                             |  class MainClassRunner extends ${traitName} {}
                             |  private var argsOpt0 = Option.empty[Seq[String]]
                             |  def setArgs(args: Seq[String]): Unit = {
                             |    argsOpt0 = Some(args)
                             |  }
                             |  def argsOpt: Option[Seq[String]] = argsOpt0
                             |  def args: Seq[String] = argsOpt.getOrElse {
                             |    sys.error("No arguments passed to this script")
                             |  }
                             |  def main(args: Array[String]): Unit = {
                             |    setArgs(args)
                             |    new MainClassRunner()
                             |  }
                             |}
                             |""".stripMargin

    val packageDirective =
      if (pkgName.isEmpty) "" else s"package ${AmmUtil.encodeScalaSourcePath(pkgName)}" + "\n"

    // indentation is important in the generated code, so we don't want scalafmt to touch that
    // format: off
    val top = AmmUtil.normalizeNewlines(s"""$packageDirective
                                            |
                                            |trait ${traitName} {\n
                                            |""".stripMargin
    )
    val bottom = AmmUtil.normalizeNewlines(s"""\ndef args = ${name}.args\n
  $extraCode
}\n object ${indexedWrapperName.backticked} extends ${traitName}

$mainObjectCode
""")
    // format: on

    (top, bottom, userCodeNestingLevel)
  }
}

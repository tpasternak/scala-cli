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
    val name = mainClassObject(indexedWrapperName).backticked

    val packageDirective =
      if (pkgName.isEmpty) "" else s"package ${AmmUtil.encodeScalaSourcePath(pkgName)}" + "\n"

    // indentation is important in the generated code, so we don't want scalafmt to touch that
    // format: off
    val top = AmmUtil.normalizeNewlines(s"""$packageDirective
                                            |
                                            |trait ${indexedWrapperName.backticked}_trait {\n
                                            |""".stripMargin
    )
    val bottom = AmmUtil.normalizeNewlines(s"""\ndef args = ${name}.args\n
  $extraCode
}\n object ${indexedWrapperName.backticked} extends ${indexedWrapperName.backticked}_trait

""")
    // format: on

    (top, bottom, userCodeNestingLevel)
  }
}

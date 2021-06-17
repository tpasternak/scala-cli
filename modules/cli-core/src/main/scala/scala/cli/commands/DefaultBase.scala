package scala.cli.commands

import caseapp._

class DefaultBase(defaultHelp: => String) extends ScalaCommand[RunOptions] {
  override def group = "Main"
  override def sharedOptions(options: RunOptions) = Some(options.shared)
  private[cli] var anyArgs = false
  def run(options: RunOptions, args: RemainingArgs): Unit =
    if (anyArgs)
      Run.run(options, args, defaultInputs = None)
    else
      println(defaultHelp)
}

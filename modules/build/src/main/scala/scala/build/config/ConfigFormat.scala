package scala.build.config

import pureconfig.ConfigReader
import pureconfig.generic.semiauto._

import scala.build.Build
import scala.build.options.{BuildOptions, ClassPathOptions, JavaOptions, ScalaOptions}
import dependency.parser.DependencyParser

final case class ConfigFormat(
  scala: Scala = Scala(),
  scalaJs: ScalaJs = ScalaJs(),
  jvm: Option[String] = None,
  java: Java = Java(),
  dependencies: List[String] = Nil,
  repositories: List[String] = Nil
) {
  def buildOptions: BuildOptions =
    BuildOptions(
      scalaOptions = ScalaOptions(
        scalaVersion = scala.version,
        scalaBinaryVersion = scala.binaryVersion,
        scalacOptions = scala.options
      ),
      javaOptions = JavaOptions(
        javaHomeOpt = java.home,
        jvmIdOpt = jvm
      ),
      classPathOptions = ClassPathOptions(
        extraDependencies = dependencies.filter(_.nonEmpty).map { depStr =>
          DependencyParser.parse(depStr) match {
            case Left(err) => sys.error(s"Error parsing dependency '$depStr': $err")
            case Right(dep) => dep
          }
        },
        extraRepositories = repositories.filter(_.nonEmpty)
      )
    )
}

object ConfigFormat {
  implicit val reader: ConfigReader[ConfigFormat] = deriveReader
}

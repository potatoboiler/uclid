package uclid.svcomp

import java.nio.file.{Files, Path}
import scala.collection.mutable.MutableList
import scala.collection.immutable
import java.io.File
import java.nio.file.StandardOpenOption
import uclid.lang
import uclid.lang._

abstract class SupportedLanguages()
object SupportedLanguages {
  case object C89 extends SupportedLanguages() // CBMC
  case object C99 extends SupportedLanguages() // CBMC
  case object C11 extends SupportedLanguages() // CBMC
  case object Cpp98 extends SupportedLanguages() // CBMC
  case object Cpp03 extends SupportedLanguages() // CBMC
  case object Cpp11 extends SupportedLanguages() // CBMC

  def mapStringToLang(s: String): SupportedLanguages = {
    s match {
      case "C"   => C11
      case "Cpp" => Cpp11
      case _     => ???
    }
  }

}

abstract class SupportedVerifiers() {
  // TODO: Any should be the most restrictive Type over Uclid.lang.Types
  // Use typeclasses? https://stackoverflow.com/questions/55700613/mapped-types-in-scala
  val type_mapping: immutable.Map[Any, String]
  def convert_uclid_types(input_type: Type): String
  def check_module(module: lang.Module): Unit
  def invoke(cbmc_filepath: Path): Unit
}

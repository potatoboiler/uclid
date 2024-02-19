package uclid.svcomp

import java.nio.file.{Files, Path}
import scala.collection.mutable.MutableList
import scala.collection.immutable
import scala.sys.process._
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
  type TraceType = Boolean

  // TODO: Any should be the most restrictive Type over Uclid.lang.Types
  // Use typeclasses? https://stackoverflow.com/questions/55700613/mapped-types-in-scala
  val type_mapping: immutable.Map[Any, String]
  def convert_uclid_types(input_type: Type): String
  def check_procedure(proc: ProcedureDecl, module: lang.Module): List[TraceType]
  def run(command: Seq[String]): (Int, String, String)
}
object SupportedVerifiers {
  type ResultType = Option[Unit]

  def mapStringToVerifier(s: String): SupportedVerifiers = {
    s match {
      case "CBMC" => CBMC
      case _      => ???
    }
  }

  def dispatchVerifierFromProcedure(
      proc: ProcedureDecl,
      module: Module
  ): SupportedVerifiers.ResultType = {
    val traces = proc.verifier.map {
      _ match {
        case CBMC => CBMC.check_procedure(proc, module)
        case _    => ???
      }
    }
    if (traces.nonEmpty) {
      Some(())
    } else {
      None
    }
  }

  /*
   * The run function accepts a command to run as a Sequence of strings, runs it, and returns the command return status,
   * stdout output, and stderr output.
   * Precondition: The Sequence of strings must be non-empty.
   */
  def run(command: Seq[String]): (Int, String, String) = {
    val output = new StringBuilder
    val error = new StringBuilder

    val status = command ! ProcessLogger(
      line => output append (line + "\n"),
      line => error append (line + "\n")
    )

    if (output.nonEmpty) { output.setLength(output.length - 1) }
    if (error.nonEmpty) { error.setLength(error.length - 1) }

    (status, output.toString, error.toString)
  }
}

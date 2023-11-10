package uclid.svcomp

import java.nio.file.{Files, Path}
import scala.collection.mutable.MutableList
import scala.collection.immutable
import uclid.lang.UclidParser
import uclid.lang.CBlock
import uclid.lang.ProcedureDecl
import uclid.lang.{
  Type,
  BooleanType,
  IntegerType,
  RealType,
  FloatType,
  ArrayType,
  EnumType,
  BitVectorType,
  StringType,
  UninterpretedType, // void* ?
  TupleType, // struct?
  GroupType // struct or array?
}
import _root_.java.io.File
import java.nio.file.StandardOpenOption
import _root_.uclid.lang.Identifier

sealed abstract class SupportedLanguages()
object SupportedLanguages {
  case object C89 extends SupportedLanguages() // CBMC
  case object C99 extends SupportedLanguages() // CBMC
  case object C11 extends SupportedLanguages() // CBMC
  case object Cpp98 extends SupportedLanguages() // CBMC
  case object Cpp03 extends SupportedLanguages() // CBMC
  case object Cpp11 extends SupportedLanguages() // CBMC
}

sealed abstract class SupportedVerifiers() {
  // TODO: Any should be the most restrictive Type over Uclid.lang.Types
  // Use typeclasses? https://stackoverflow.com/questions/55700613/mapped-types-in-scala
  val type_mapping: immutable.Map[Any, String]
  def convert_uclid_types(input_type: Type): String
  def invoke(): Unit
}
object SupportedVerifiers {
  case object CBMC extends SupportedVerifiers() {
    var c_functions: MutableList[ProcedureDecl] = MutableList()

    override def convert_uclid_types(input_type: Type): String = {
      input_type match {
        case BooleanType() | IntegerType() | RealType() =>
          type_mapping(input_type)
        case _ => ???
      }
    }

    override val type_mapping =
      Map(
        BooleanType -> "bool",
        IntegerType -> "int",
        RealType -> "double",
        FloatType -> "float",
        ArrayType -> ???,
        EnumType -> ???,
        BitVectorType -> ???,
        StringType -> ???,
        UninterpretedType -> ???,
        TupleType -> ???,
        GroupType -> ???
      )
    def add_cfunc(c_procedure: ProcedureDecl): Unit = {
      if (!c_procedure.body.isInstanceOf[CBlock]) {
        return
      }

      c_functions += c_procedure
    }

    // TODO: find an appropriate place to call this function
    def generate_cbmc_file(): Path = {
      val temp_file_path: Path =
        Files.createTempFile(
          System.getProperty("user.dir"),
          ".c",
          null
        ) // TODO: prefix/suffix set to original input file name

      val temp_file_handle =
        Files.newOutputStream(temp_file_path, StandardOpenOption.CREATE_NEW)

      val main_entries = MutableList[String]()
      val function_entries = MutableList[String]()
      for (c_function: ProcedureDecl <- c_functions) {
        assert(
          c_function.sig.outParams.length == 1,
          s"Expected 1 C return/output value, instead got ${c_function.sig.outParams.length}"
        )
        val return_val = (
          c_function.sig.outParams.head._1,
          convert_uclid_types(c_function.sig.outParams.head._2)
        )
        val parameter_vals =
          c_function.sig.inParams.map((t: (Identifier, Type)) =>
            (t._1, convert_uclid_types(t._2))
          )

        val input_decls = parameter_vals
          .map((t: (Identifier, String)) => s"${t._2} ${t._1};")
          .mkString(" ")

        val preconditions = c_function.requires.map(e => ???) // TODO
        val postconditions = c_function.ensures.map(e => ???) // TODO

        val cprover_assume_conditions = ??? // TODO
        val cprover_requires_conditions = ??? // TODO
        val cprover_ensures_conditions = ??? // TODO

        val function_name = c_function.id
        val function_args = parameter_vals.map((t: (Identifier, String)) =>
          s"${t._2} ${t._1.name}"
        )
        val function_body = c_function.body.toString()

        val function_invocation =
          function_name.name + s"(${parameter_vals
              .map((t: (Identifier, String)) => t._1.name)
              .mkString(",")});"

        function_entries += s"""$return_val $function_name ($function_args)
        { 
          $cprover_requires_conditions 
          $function_body 
          $cprover_ensures_conditions 
        }"""
        main_entries += s"{ $input_decls \n $cprover_assume_conditions \n $function_invocation }"
      }

      temp_file_handle.write("#include <stdlib.h>\n\n".getBytes())
      function_entries.foreach((function_def: String) =>
        temp_file_handle.write(function_def.getBytes())
      )
      temp_file_handle.write(
        s"int main() { ${main_entries.mkString("\n")} }".getBytes()
      )

      temp_file_path
    }

    override def invoke() = ??? // TODO
  }
}

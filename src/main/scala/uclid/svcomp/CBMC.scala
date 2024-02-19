package uclid.svcomp

import java.nio.file.{Files, Path}
import scala.collection.mutable.{MutableList, Set}
import scala.collection.immutable
import java.io.File
import java.nio.file.StandardOpenOption
import uclid.lang
import uclid.lang._
import java.util.Collection
import scala.collection.mutable.Queue

case object CBMC extends SupportedVerifiers() {
  type RawTraceType = scala.collection.AbstractSeq[ujson.Value]

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

  override def check_procedure(
      proc: ProcedureDecl,
      module: lang.Module
  ): List[TraceType] = {
    def parse_trace(t: RawTraceType): TraceType = { ??? }

    val cbmc_filepath = generate_cbmc_file(module, List(proc))

    val (status, output, error) = invoke(cbmc_filepath)
    val parsed_output = parseCbmcOutput(output)

    val counterexample_list = MutableList[TraceType]()
    for ((procedure_name, (result, trace)) <- parsed_output) {
      // TODO: use the counterexample trace to refine
      if (!result) {
        counterexample_list += parse_trace(trace)
      }
    }

    counterexample_list.toList
  }

  /** Traverses module hierarchy and translates referenced modules into C
    * structs
    *
    * @param root_module
    * @return
    *   translated module
    */
  def convert_uclid_module_hierarchy(root_module: Module): String = {
    var referenced_modules = Queue[Module]()
    var translated_modules = MutableList[String]()

    /** Translates a UCLID module to a C struct definition. Used for verifying
      * procedures that contain references to module variables.
      *
      * @param module
      * @return
      */
    def convert_uclid_module(module: Module): String = {
      var converted_decls = List[String]()

      // Temporary measure until we get recursion and higher order types working properly
      def convert_id_type(tuple: (Identifier, Type)): String = {
        val (id, typ) = tuple

        // TODO: refactor to use first-party toString methods instead of this
        s"${typ.getClass().getName()} ${id.name};"
      }

      converted_decls ++= module.inputs.map(convert_id_type)
      converted_decls ++= module.outputs.map(convert_id_type)

      for (const_decl <- module.constantDecls) {} // TODO
      for (const_decl <- module.constImportDecls) {} // TODO
      converted_decls ++= module.vars.map(convert_id_type)
      converted_decls ++= module.sharedVars.map(convert_id_type)
      converted_decls ++= module.constants.map(convert_id_type)

      for ((id, value) <- module.constLits) {}

      for (imported_module <- module.moduleImportDecls) {
        // TODO: add a ctx parameter to invocation?
        // Note: context is determined at compile time, can we parameterize the instantiation of
      }
      for (imported_function <- module.funcImportDecls) {
        // TODO
      }

      s"struct ${module.id} { ${converted_decls.mkString(";")} };"
    }

    translated_modules.mkString("\n\n")
  }

  def convert_uclid_expr(e: Expr): String = {
    val converted_expr: String = e match {
      // TODO: EnumType, BitVectorType, ArrayType, GroupType
      // TODO: case match on sizes of sig and exp for floats to differentiate double from float

      case FreshLit(typ) => ???

      case BoolLit(value)                           => value.toString()
      case IntLit(value)                            => value.toString()
      case RealLit(integral, fractional)            => e.toString()
      case FloatLit(integral, fractional, exp, sig) => e.toString()
      case BitVectorLit(value, width)               => ???
      case StringLit(value)                         => e.toString()
      case ConstArray(exp, typ)                     => ???
      case UninterpretedTypeLiteral(value)          => ???
      case ConstRecord(fieldvalues)                 => ???
      case Tuple(values)                            => ???

      case OperatorApplication(op, operands) =>
        // TODO: BitVector operators, ArrayUpdate, RecordUpdate, GetNextValueOp, DistinctOp, anything Temporal
        op match {
          // extends BooleanOperator
          case ImplicationOp() => {
            assert(operands.length == 2)
            val op0 = convert_uclid_expr(operands(0))
            val op1 = convert_uclid_expr(operands(1))
            s"!$op0 || $op1"
          }
          case IffOp() => {
            assert(operands.length == 2)
            val op0 = convert_uclid_expr(operands(0))
            val op1 = convert_uclid_expr(operands(1))
            s"(!$op0 || $op1) && ($op0 || !$op1)"
          }

          case
              // extends PolymorphicOperator
              LTOp() | LEOp() | GTOp() | GEOp() | AddOp() | SubOp() | MulOp() |
              DivOp()
              // extends IntArgOperator
              | IntLTOp() | IntLEOp() | IntGTOp() | IntGEOp() | IntAddOp() |
              IntSubOp() | IntMulOp() | IntDivOp()
              // extends RealArgOperator
              | RealLTOp() | RealLEOp() | RealGTOp() | RealGEOp() |
              RealAddOp() | RealSubOp() | RealMulOp() | RealDivOp()
              // extends FPArgOperator
              | FPLTOp(_, _) | FPLEOp(_, _) | FPGTOp(_, _) | FPGEOp(_, _) |
              FPAddOp(_, _) | FPSubOp(_, _) | FPMulOp(_, _) | FPDivOp(_, _)
              // extends BooleanOperator
              | DisjunctionOp() | ConjunctionOp()
              // extends ComparisonOperator
              | EqualityOp() | InequalityOp() => {
            assert(operands.length == 2)
            val op0 = convert_uclid_expr(operands(0))
            val op1 = convert_uclid_expr(operands(1))
            s"$op0 ${op.toString()} $op1"
          }

          case UnaryMinusOp() | IntUnaryMinusOp() | RealUnaryMinusOp() |
              FPUnaryMinusOp(_, _) | NegationOp() =>
            op.toString() + convert_uclid_expr(operands(0))

          case FPIsNanOp(_, _) => {
            // https://stackoverflow.com/questions/33924866/is-x-x-a-portable-way-to-test-for-nan
            assert(operands.length == 1)
            val operand = convert_uclid_expr(operands(0))
            s"$operand != $operand"
          }

          case _ => ???
        }

      case Identifier(name)                           => name
      case ExternalIdentifier(moduleId, id)           => ???
      case IndexedIdentifier(name, indices)           => ???
      case QualifiedIdentifier(f, typs)               => ???
      case QualifiedIdentifierApplication(qid, exprs) => ???

      case FuncApplication(e, args) => ???
      case Lambda(ids, e)           => ???
      case LetExpr(ids, e)          => ???
    }

    s"( $converted_expr )"
  }

  // TODO: find an appropriate place to call this function
  def generate_cbmc_file(
      module: Module,
      c_functions: Iterable[ProcedureDecl]
  ): Path = {
    val temp_file_path: Path =
      Files.createTempFile(
        "uclid.cbmc." + module.id,
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

      val preconditions = c_function.requires.map(convert_uclid_expr)
      val postconditions = c_function.ensures.map(convert_uclid_expr)

      val cprover_assume_conditions = preconditions
        .map((precondition) => s"__CPROVER_assume($precondition);")
        .mkString("\n")
      val cprover_requires_conditions = preconditions
        .map((precondition) => s"__CPROVER_precondition($precondition);")
        .mkString("\n")
      val cprover_ensures_conditions = postconditions
        .map((postcondition) => s"__CPROVER_postcondition($postcondition);")
        .mkString("\n")

      val function_name = c_function.id
      val function_args =
        parameter_vals.map((t: (Identifier, String)) => s"${t._2} ${t._1.name}")
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

      // TODO: deprecate this, so that verifyProcedure does indeed operate procedure by procedure?

      main_entries += s"{ $input_decls \n $cprover_assume_conditions \n $function_invocation }"
    }

    temp_file_handle.write("#include <stdlib.h>\n\n".getBytes())
    temp_file_handle.write(convert_uclid_module_hierarchy(module).getBytes())
    function_entries.foreach((function_def: String) =>
      temp_file_handle.write(function_def.getBytes())
    )
    temp_file_handle.write(
      s"int main() { ${main_entries.mkString("\n")} }".getBytes()
    )

    temp_file_path
  }

  def invoke(cbmc_filepath: Path): (Int, String, String) = {
    val cmd =
      "cbmc" :: cbmc_filepath.toString :: "--json-ui" :: "--verbosity" :: "10" :: Nil
    return SupportedVerifiers.run(cmd)
  }

  override def run(command: Seq[String]): (Int, String, String) =
    SupportedVerifiers.run(command)

  /*
   * The parseCbmcOutput function takes the output of running a cbmc command (as a string which is a list of json strings) and returns a hashmap of
   * test description string to a tuple of test result (as a boolean) and counterexample trace (as a list of json objects, if
   * the test failed).
   *
   * Raises an exception if the output is ill-formed. This can be because:
   * 1. The output string is not a list of valid json strings.
   * 2. It can't find a json string with the key "result" in the list.
   * 3. The value of the "result" key is not a list of json objects, each of which has a "description" key and a "status" key, which can only be "SUCCESS" or "FAILURE". Furthermore, if the "status" key is "FAILURE", then the json object must also have a "trace" key whose value is a list of json objects.
   */
  // @`throws`[Exception]("If the output is ill-formed.")
  def parseCbmcOutput(output: String): Map[String, (Boolean, RawTraceType)] = {
    // TODO: rewrite using org.json4s components?
    ujson.read(output).arr.find(_.obj.contains("result")) match {
      case Some(json) =>
        val result = json("result")
        result.arr.map { testResult =>
          val description = testResult("description").str
          val status = testResult("status").str
          val trace = if (status == "FAILURE") { testResult("trace").arr }
          else List()
          (description, (status == "SUCCESS", trace))
        }.toMap
      case None =>
        throw new Exception(
          "Could not find a json string with the key \"result\" in the output."
        )
    }
  }
}

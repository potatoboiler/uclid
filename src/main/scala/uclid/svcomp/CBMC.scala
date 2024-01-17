package uclid.svcomp

import java.nio.file.{Files, Path}
import scala.collection.mutable.{MutableList, Set}
import scala.collection.immutable
import java.io.File
import java.nio.file.StandardOpenOption
import uclid.lang
import uclid.lang._

case object CBMC extends SupportedVerifiers() {
  // var c_functions: Map[lang.Module, Set[ProcedureDecl]] = Map()
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
  def add_cfunc(c_procedure: ProcedureDecl, module: Module): Unit = {
    if (!c_procedure.body.isInstanceOf[CBlock]) {
      return
    }
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

      case Identifier(name, None)                     => name
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
    function_entries.foreach((function_def: String) =>
      temp_file_handle.write(function_def.getBytes())
    )
    temp_file_handle.write(
      s"int main() { ${main_entries.mkString("\n")} }".getBytes()
    )

    temp_file_path
    ??? // todo: rewrite for new semantics
  }

  override def check(module: lang.Module) = {
    ???
  }

  override def invoke(cbmc_filepath: Path, entrypoint: String) = {
    val cmd = List("cbmc", cbmc_filepath.toString, "--json-ui", "--verbosity", "10")
    val status, out, err = SupportedVerifiers.run(cmd)

    ???
  }
}

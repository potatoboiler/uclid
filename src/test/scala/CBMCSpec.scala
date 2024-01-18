package uclid
package test

import org.scalatest.flatspec.AnyFlatSpec

object CBMCSpec {}
class CBMCSpec extends AnyFlatSpec {
  "test-cbmc.ucl" should "verify successfully." in {
    VerifierSpec
  }
}

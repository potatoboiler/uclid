package uclid
package test

import org.scalatest.flatspec.AnyFlatSpec

object CBMCSpec {
  def expectedFails(filename: String, nFail: Int): String = {
    UclidMain.enableStringOutput()
    UclidMain.clearStringOutput()
    val config = UclidMain.Config().copy(smtSolver = List("delphi", "--smto"))
    val modules = UclidMain.compile(
      ConfigCons.createConfig(filename),
      lang.Identifier("main"),
      true
    )
    val mainModule =
      UclidMain.instantiate(config, modules, lang.Identifier("main"))
    assert(mainModule.isDefined)
    val results = UclidMain.execute(mainModule.get, config)
    val outputString = UclidMain.stringOutput.toString()
    assert(results.count((e) => e.result.isFalse) == nFail)

    assert(results.count((e) => e.result.isUndefined) == 0)
    outputString
  }
}
class CBMCSpec extends AnyFlatSpec {
  "test-cbmc.ucl" should "verify successfully." in {
    CBMCSpec.expectedFails("./test/test-cbmc.ucl", 0)
  }
}

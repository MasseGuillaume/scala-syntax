package org.scalafmt.tests

import scala.meta.Lit

object CoverageSuite extends BaseScalaPrinterTest {

  // scalameta#1324 (cannot parse Z and S)
  check(Lit.Byte(2), "2Z")         // Lit.Byte
  check(Lit.Short(2), "2S")        // Lit.Short

  checkResource("coverage.scala")
}

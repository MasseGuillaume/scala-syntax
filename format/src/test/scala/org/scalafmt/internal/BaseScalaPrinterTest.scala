package org.scalafmt.internal

import scala.meta.dialects
import scala.meta.parsers.Parse
import org.scalafmt.Format
import org.scalafmt.InternalOptions
import org.scalafmt.Options
import org.scalameta.logger

abstract class BaseScalaPrinterTest extends DiffSuite {

  val defaultOptions = InternalOptions(100).copy(
    dialect = dialects.Sbt1.copy(
      allowTypeLambdas = true,
      allowAndTypes = true,
      allowImplicitFunctionTypes = true
    ),
    parser = Parse.parseStat
  )
  def checkType(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseType))
  }
  def checkPat(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parsePat))
  }

  def checkCase(
      original: String,
      options: InternalOptions = defaultOptions
  ): Unit = {
    check(original, options.copy(parser = Parse.parseCase))
  }

  def check(original: String, options: Options = defaultOptions): Unit = {
    check(original, original, options)
  }

  def check(original: String, expected: String): Unit = {
    check(original, expected, defaultOptions)
  }

  def check(
      original2: String,
      expected2: String,
      options: Options
  ): Unit = {
    val original = original2.replace("'''", "\"\"\"")
    val expected = expected2.replace("'''", "\"\"\"")
    test(logger.revealWhitespace(original)) {
      val obtained = Format.format(original, options)
      import scala.meta._
      assertNoDiff(obtained, expected)
    }
  }
}

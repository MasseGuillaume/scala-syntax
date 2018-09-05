package org.scalafmt.internal

import org.scalafmt.tests.BaseScalaPrinterTest

import scala.meta._

trait PreserveComments { _: BaseScalaPrinterTest =>
  def extractComments(tree: Tree): String = {
    val nl = "\n"
    val sep = "--------------------------------------"
    val comments =
      tree.tokens.collect {
        case Token.Comment(content) => content
      }
    comments.mkString("", nl + sep + nl, nl)
  }

  def noDiff(
      relativePath: String,
      originalComments: String,
      formattedComments: String
  ): PropertyResult = {
    if (originalComments != formattedComments) {

      println(originalComments)
      println("----")
      println(formattedComments)

      val maxSizeForDiff = 1000
      if (originalComments.size < maxSizeForDiff && formattedComments.size < maxSizeForDiff) {
        val diff = unified(relativePath, originalComments, formattedComments)
        if (diff.isEmpty) PropertyResult.Success
        else PropertyResult.Failure(diff)
      } else PropertyResult.Failure("-- no diff --")
    } else PropertyResult.Success
  }

  def check(input: Input, relativePath: String): PropertyResult = {
    val originalTree = input.parse[Source].get
    val originalComments = extractComments(originalTree)

    val formattedComments = {
      val formatted = prettyPrint(originalTree)
      val formattedTree = formatted.parse[Source].get
      extractComments(formattedTree)
    }

    noDiff(relativePath, originalComments, formattedComments)
  }
}

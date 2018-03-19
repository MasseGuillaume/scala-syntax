package org.scalafmt.internal

import org.scalafmt.internal.TokensOps._
import scala.meta.internal.format.Comments._

import scala.meta._
import scala.meta.Token
import scala.meta.Token._
import scala.meta.contrib._

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.{text, empty, space, lineNoFlat}

import org.scalameta.logger

final case class AssociatedTrivias(
    allLeadings: Map[Token, Tokens],
    allTrailings: Map[Token, Tokens]
) {
  def leadings(token: Token): Option[Tokens] =
    allLeadings.get(token)

  def trailings(token: Token): Option[Tokens] =
    allTrailings.get(token)

  private def toDoc(tokens: Option[Seq[Token]], isLeading: Boolean): Doc = {
    tokens match {
      case Some(ts) => {
        val hasComment = ts.exists(_.is[Comment])
        if (hasComment) {
          val commentsToken =
            if (isLeading) ts.dropWhile(!_.is[Comment])
            else ts

          commentsToken.foldLeft(empty) {
            case (acc, t) => {
              val doc =
                t match {
                  case _: LF => lineNoFlat
                  case _: Space => space
                  case e => text(e.text)
                }
              acc + doc
            }
          }
        } else empty
      }
      case None => empty
    }
  }

  private def wrap(
      leadings: Option[Seq[Token]],
      doc: Doc,
      trailings: Option[Seq[Token]]
  ): Doc = {
    val leading = toDoc(leadings, isLeading = true)
    val trailing = toDoc(trailings, isLeading = false)
    leading + doc + trailing
  }

  def wrap(tree: Tree, token: => Token, doc: Doc): Doc = {
    if(tree.hasTokens) {
      wrap(leadings(token), doc, trailings(token))
    } else doc
  }

  def wrap(tree: Tree, doc: Doc): Doc = {
    if(tree.hasTokens) {
      val tokens = tree.tokens.filterNot(_.is[Trivia])
      assert(tokens.size == 1, s"expected one token, got ${tree.tokens.structure}, filtered: ${tokens.map(_.structure)}")
      val token = tokens.head
      wrap(leadings(token), doc, trailings(token))
    } else doc
  }

  def wrapTrailing(tree: Tree, doc: Doc): Doc =
    if(tree.hasTokens) {
      val tokens = tree.tokens.filterNot(_.is[Trivia])
      wrap(None, doc, trailings(tokens.last))
    } else doc

  // def wrap(tree: Tree, firstToken: => Token, lastToken: => Token, doc: Doc): Doc = {
  //   if (tree.hasTokens) wrap(leadings(firstToken), doc, trailings(lastToken))
  //   else doc
  // }

  // def wrap(tree: Tree, doc: Doc): Doc = {
  //   if (tree.hasTokens) {
  //     val tokens = tree.tokens.filterNot(_.is[Trivia])
  //     val firstToken = tokens.head
  //     val lastToken = tokens.last
  //     wrap(tree, firstToken, lastToken, doc)
  //   } else doc
  // }

  private def pretty(token: Token): String = {
    if (token.is[Token.BOF]) {
      "BOF"
    } else {
      token.syntax
    }
  }
  private def pretty(association: Map[Token, Tokens]): String =
    association.toList
      .sortBy {
        case (tok, tokens) =>
          (tok.start, tok.end, tokens.start, tokens.end)
      }
      .map {
        case (tok, tokens) =>
          val tokensStructure = tokens
            .map(token => logger.revealWhitespace(pretty(token)))
            .mkString("[", ",", "]")
          s"    ${tok.structure} => $tokensStructure"
      }
      .mkString("\n")
  def syntax: String =
    s"""|AssociatedTrivias(
        |  Leading =
        |${pretty(allLeadings)}
        |  Trailing =
        |${pretty(allTrailings)}
        |)""".stripMargin
  override def toString: String = syntax
}
object AssociatedTrivias {
  def apply(tree: Tree): AssociatedTrivias = apply(tree.tokens)
  def apply(tokens: Tokens): AssociatedTrivias = {
    val allLeadings = Map.newBuilder[Token, Tokens]
    val allTrailings = Map.newBuilder[Token, Tokens]

    var leadingStart: Option[Token] = None
    var lastToken: Option[Token] = None
    var isLeading = true

    def setTrivia(t: Token): Unit = {
      if (isLeading && leadingStart.isEmpty) {
        leadingStart = Some(t)
      }
    }

    def doTrailing(currentToken: Token): Unit = {
      lastToken.foreach { last =>
        val start = tokens.binarySearch(last).get + 1
        val includingEnd =
          if (currentToken.is[LF]) 1
          else 0
        val end = tokens.binarySearch(currentToken).get + includingEnd
        val slice = tokens.slice(start, end)

        if (slice.nonEmpty) {
          allTrailings += last -> slice
        }
      }
      lastToken = None
    }

    tokens.foreach {
      case t: Comment =>
        setTrivia(t)

      case t: Token.BOF =>
        ()

      case t: Token.EOF =>
        doTrailing(t)

      case t @ Token.LF() =>
        setTrivia(t)
        doTrailing(t)
        isLeading = true

      case t @ Trivia() =>
        setTrivia(t)

      case currentToken =>
        doTrailing(currentToken)
        leadingStart.foreach { start =>
          val slice = tokens.slice(start, currentToken)
          if (slice.nonEmpty) {
            allLeadings += currentToken -> slice
          }
        }
        leadingStart = None
        lastToken = Some(currentToken)
        isLeading = false
    }

    AssociatedTrivias(allLeadings.result(), allTrailings.result())
  }
}

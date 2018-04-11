package org.scalafmt.internal
package tokens

import org.scalafmt.internal.{ScalaToken => S}

import scala.meta
import scala.meta._
import scala.meta.tokens.Token._

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc.empty

object SyntaxTokensTerm {
  import Term._
  import SyntaxTokensUtils._

  implicit class XtensionTermAnnotateSyntax(private val tree: Annotate)
      extends AnyVal {
    def tokensAt: At = tree.findAfter[At](_.expr).get
  }

  implicit class XtensionTermAscribeSyntax(private val tree: Ascribe)
      extends AnyVal {
    def tokensColon: Colon = tree.findAfter[Colon](_.expr).get
  }

  implicit class XtensionTermApplySyntax(private val tree: Apply)
      extends AnyVal {
    def `(`(implicit trivia: AssociatedTrivias): Doc = {
      tokensLeftParen.map(lp => trivia.wrap(tree, lp, S.`(`)).getOrElse(empty)
    }
    def tokensLeftParen: Option[LeftParen] =
      if (tree.args.nonEmpty) tree.findBetween[LeftParen](_.fun, _.args.head)
      else tree.findAfter[LeftParen](_.fun)

    def `)`(implicit trivia: AssociatedTrivias): Doc = {
      tokensRightParen.map(rp => trivia.wrap(tree, rp, S.`)`)).getOrElse(empty)
    }
    def tokensRightParen: Option[RightParen] =
      if (tree.args.nonEmpty) tree.findAfter[RightParen](_.args.last)
      else tree.findAfter[RightParen](_.fun)
    def tokensLeftBrace: Option[LeftBrace] = {
      tree.args match {
        case List(b: Block) => Some(b.find[LeftBrace].get)
        case _ => None
      }
    }
    def `{`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensLeftBrace.get, S.`{`)
    }
    def tokensRightBrace: Option[RightBrace] = {
      tree.args match {
        case List(b: Block) => 
          if(b.stats.nonEmpty) Some(b.findAfter[RightBrace](_.stats.last).get)
          else Some(b.find[RightBrace].get)
        case _ => None
      }
    }
    def `}`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensRightBrace.get, S.`}`)
    }
  }

  implicit class XtensionTermApplyInfixSyntax(private val tree: ApplyInfix)
      extends AnyVal {
    def `(`(implicit trivia: AssociatedTrivias): Doc = {
      tokensLeftParen.map(lp => trivia.wrap(tree, lp, S.`(`)).getOrElse(empty)
    }

    def tokensLeftParen: Option[LeftParen] = {
      if (tree.args.nonEmpty) tree.findBetween[LeftParen](_.op, _.args.head)
      else tree.findAfter[LeftParen](_.op)
    }

    def `)`(implicit trivia: AssociatedTrivias): Doc = {
      tokensRightParen.map(rp => trivia.wrap(tree, rp, S.`)`)).getOrElse(empty)
    }
    def tokensRightParen: Option[RightParen] = {
      import org.scalafmt.internal.TokenOps._
      println(tree.tokens.toList.map(_.showClass))

      val tt = tree.after(_.args.last, tree.tokens).get
      println(tt)

      if (tree.args.nonEmpty) tree.findAfter[RightParen](_.args.last)
      else tree.findAfter[RightParen](_.op)
    }

    def tokensLeftBrace: Option[LeftBrace] = {
      tree.args match {
        case List(b: Block) => 
          if(b.stats.nonEmpty) Some(b.findAfter[LeftBrace](_.stats.head).get)
          else Some(b.find[LeftBrace].get)
        case _ => None
      }
    }
    def `{`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensLeftBrace.get, S.`{`)
    }
    def tokensRightBrace: Option[RightBrace] = {
      tree.args match {
        case List(b: Block) => 
          if(b.stats.nonEmpty) Some(b.findAfter[RightBrace](_.stats.last).get)
          else Some(b.find[RightBrace].get)
        case _ => None
      }
    }
    def `}`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensRightBrace.get, S.`}`)
    }
  }

  implicit class XtensionTermApplyTypeSyntax(private val tree: ApplyType)
      extends AnyVal {
    def tokensLeftBracket: LeftBracket =
      tree.findBetween[LeftBracket](_.fun, _.targs.head).get
    def tokensRightBracket: RightBracket =
      tree.findAfter[RightBracket](_.targs.last).get
  }

  implicit class XtensionTermAssignSyntax(private val tree: Assign)
      extends AnyVal {
    def tokensEqual: Equals = tree.findAfter[Equals](_.lhs).get
  }

  implicit class XtensionTermBlockSyntax(private val tree: Block)
      extends AnyVal {
    def tokensLeftBrace: LeftBrace = blockStartBrace(tree)
    def `{`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensLeftBrace, S.`{`)
    }
    def tokensRightBrace: RightBrace = blockEndBrace(tree)(_.stats)
    def `}`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensRightBrace, S.`}`)
    }
  }

  implicit class XtensionTermDoSyntax(private val tree: Do) extends AnyVal {
    def tokensDo: KwDo = tree.find[KwDo].get
    def tokensWhile: KwWhile = tree.findBetween[KwWhile](_.body, _.expr).get
    def tokensLeftParen: LeftParen =
      tree.findBetween[LeftParen](_.body, _.expr).get
    def tokensRightParen: RightParen = tree.findAfter[RightParen](_.expr).get
  }

  implicit class XtensionTermEtaSyntax(private val tree: Eta) extends AnyVal {
    def tokensUnderscore: Underscore = tree.find[Underscore].get
  }

  implicit class XtensionTermForSyntax(private val tree: For) extends AnyVal {
    def tokensFor: KwFor = tree.find[KwFor].get
  }

  implicit class XtensionTermForYieldSyntax(private val tree: ForYield)
      extends AnyVal {
    def tokensFor: KwFor = tree.find[KwFor].get
    def tokensYield: KwYield = tree.findAfter[KwYield](_.enums.last).get
  }

  implicit class XtensionTermFunctionSyntax(private val tree: Function)
      extends AnyVal {
    def tokensRightArrow: RightArrow =
      tree.findAfter[RightArrow](_.params.last).get
    def `=>`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensRightArrow, S.`=>`)
    }
  }

  implicit class XtensionTermIfSyntax(private val tree: If) extends AnyVal {
    def tokensIf: KwIf = tree.find[KwIf].get
    def tokensLeftParen: LeftParen = tree.find[LeftParen].get
    def tokensRightParen: RightParen = tree.findAfter[RightParen](_.cond).get
    def tokensElse: Option[KwElse] = tree.findAfter[KwElse](_.thenp)
  }

  implicit class XtensionTermInterpolateSyntax(private val tree: Interpolate)
      extends AnyVal {
    def tokensStartQuote: Interpolation.Start =
      tree.find[Interpolation.Start].get
    def tokensEndQuote: Interpolation.End = tree.find[Interpolation.End].get
  }

  implicit class XtensionTermPartialFunctionSyntax(val tree: PartialFunction)
      extends AnyVal {
    def tokensLeftBrace: LeftBrace = blockStartBrace(tree)
    def `{`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensLeftBrace, S.`{`)
    }
    def tokensRightBrace: RightBrace = blockEndBrace(tree)(_.cases)
    def `}`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensRightBrace, S.`}`)
    }
  }

  implicit class XtensionTermMatchSyntax(private val tree: Match)
      extends AnyVal {
    def tokensMatch: KwMatch = tree.findAfter[KwMatch](_.expr).get
  }

  implicit class XtensionTermNewSyntax(private val tree: New) extends AnyVal {
    def tokensNew: KwNew = tree.find[KwNew].get
  }

  implicit class XtensionTermNewAnonymousSyntax(private val tree: NewAnonymous)
      extends AnyVal {
    def tokensNew: KwNew = tree.find[KwNew].get
  }

  implicit class XtensionTermPlaceholderSyntax(private val tree: Placeholder)
      extends AnyVal {
    def tokensUnderscore: Underscore = tree.find[Underscore].get
  }

  implicit class XtensionTermParamSyntax(private val tree: Param)
      extends AnyVal {
    def tokenColon: Option[Colon] =
      tree.decltpe.map(
        decltpe => tree.findBetween[Colon](_.name, _ => decltpe).get
      )
    def tokenEqual: Option[Equals] =
      tree.default.map(
        default =>
          tree.decltpe match {
            case Some(decltpe) =>
              tree.findBetween[Equals](_ => decltpe, _ => default).get
            case None => tree.findAfter[Equals](_.name).get
        }
      )
  }

  implicit class XtensionTermRepeatedSyntax(private val tree: Repeated)
      extends AnyVal {
    def tokensColon: Colon = tree.findAfter[Colon](_.expr).get
    def tokensUnderscore: Underscore = tree.findAfter[Underscore](_.expr).get
    def tokensAsterix: Token = tree.findAfter(isAsterisk, _.expr).get
  }

  implicit class XtensionTermReturnSyntax(private val tree: Return)
      extends AnyVal {
    def tokensReturn: KwReturn = tree.find[KwReturn].get
  }

  implicit class XtensionTermSelectSyntax(private val tree: Select)
      extends AnyVal {
    def tokensDot: Dot = tree.findAfter[Dot](_.qual).get

    def `.`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensDot, S.`.`)
    }    
  }

  implicit class XtensionTermSuperSyntax(private val tree: Super)
      extends AnyVal {
    def tokensDot: Option[Dot] = {
      tree.thisp match {
        case _: meta.Name.Anonymous => None
        case _ => tree.find[Dot]
      }
    }
  }

  implicit class XtensionTermThisSyntax(private val tree: This) extends AnyVal {
    def tokensDot: Option[Dot] = {
      tree.qual match {
        case _: meta.Name.Anonymous => None
        case _ => tree.findAfter[Dot](_.qual)
      }
    }

    def `.`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensDot.get, S.`.`)
    }

    def tokensThis: KwThis = tree.findAfter[KwThis](_.qual).get

    def `this`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensThis, S.`this`)
    }
  }

  implicit class XtensionTermThrowSyntax(private val tree: Throw)
      extends AnyVal {
    def tokensThrow: KwThrow = tree.find[KwThrow].get
  }

  implicit class XtensionTermTrySyntax(private val tree: Try) extends AnyVal {
    def tokensCatch: Option[KwCatch] =
      tree.catchp.headOption
        .map(catchp => tree.findBetween[KwCatch](_.expr, _ => catchp).get)
    def tokensFinally: Option[KwFinally] =
      tree.finallyp.map { finallyp =>
        val left = tree.catchp.lastOption.getOrElse(tree.expr)
        tree.findBetween[KwFinally](_ => left, _ => finallyp).get
      }
    def tokensLeftParen: Option[LeftParen] = tree.findBefore[LeftParen](_.expr)
    def tokensRightParen: Option[RightParen] = tree.findBetween[RightParen](
      afterP = _.expr,
      beforeP = t =>
        // scalameta#1423 currently, it's possible that both catchp and finallyp are empty
        t.catchp.headOption.getOrElse(
          t.finallyp.getOrElse(
            sys.error(
              "try catchp and finallyp are empty, this should not be possible"
            )
          )
      )
    )
    def tokensTry: KwTry = tree.find[KwTry].get
  }

  implicit class XtensionTermTryWithHandlerSyntax(
      private val tree: TryWithHandler
  ) extends AnyVal {
    def tokensCatch: KwCatch = tree.findBetween[KwCatch](_.expr, _.catchp).get
    def tokensFinally: Option[KwFinally] =
      tree.finallyp.map(
        finallyp =>
          tree
            .findBetween[KwFinally](
              _.catchp,
              _ => finallyp
            )
            .get
      )
    def tokensLeftParen: Option[LeftParen] = tree.findBefore[LeftParen](_.expr)
    def tokensRightParen: Option[RightParen] =
      tree.findBetween[RightParen](
        _.expr,
        _.catchp
      )
    def tokensTry: KwTry = tree.find[KwTry].get
  }

  implicit class XtensionTermTupleSyntax(private val tree: Tuple)
      extends AnyVal {
    def tokensLeftParen: LeftParen =
      tree.find[LeftParen].get
    def tokensRightParen: RightParen =
      tree.findAfter[RightParen](_.args.last).get
    def tokensComma: List[Comma] = commaSeparated(tree)(_.args)

    def `(`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensLeftParen, S.`(`)
    }

    def `)`(implicit trivia: AssociatedTrivias): Doc = {
      trivia.wrap(tree, tokensRightParen, S.`)`)
    }

    def `,`(implicit trivia: AssociatedTrivias): List[Doc] = {
      tokensComma.map(
        token => trivia.wrap(tree, token, S.`,`, isSeparator = true)
      )
    }
  }

  implicit class XtensionTermWhileSyntax(private val tree: While)
      extends AnyVal {
    def tokensWhile: KwWhile = tree.find[KwWhile].get
    def tokensLeftParen: LeftParen = tree.find[LeftParen].get
    def tokensRightParen: RightParen = tree.findAfter[RightParen](_.expr).get
  }

}

package scala.meta.internal.fmt

import org.scalafmt.internal._
import org.scalafmt.internal.TreeDocOps.wrapParens
import org.scalafmt.internal.TreePrinter.print

import org.typelevel.paiges.Doc

import scala.meta.{Tree, Term, Lit}

sealed trait SyntacticGroup {
  def categories: List[String]
  def precedence: Double

  def wrap(tree: Tree, side: Side = Side.Left): Doc = {
    wrap0(tree, print(tree), side)
  }
  def wrap0(tree: Tree, doc: Doc, side: Side = Side.Left): Doc = {
    val rightGroup = TreeSyntacticGroup(tree)
    wrap1(rightGroup, doc, side)
  }
  def wrap1(rightGroup: SyntacticGroup, doc: Doc, side: Side = Side.Left): Doc = {
    val leftGroup = this
    if (groupNeedsParenthesis(leftGroup, rightGroup, side)) wrapParens(doc)
    else doc
  }

  private def operatorNeedsParenthesis(
      outerOperator: String,
      innerOperator: String,
      customAssociativity: Boolean,
      customPrecedence: Boolean,
      side: Side
  ): Boolean = {
    // The associativity of an operator is determined by the operator's last character.
    // Operators ending in a colon ‘:’ are right-associative. All
    // other operators are left-associative.
    // https://www.scala-lang.org/files/archive/spec/2.13/06-expressions.html#infix-operations
    def isLeftAssociative(name: String): Boolean =
      if (customAssociativity) name.last != ':' else true

    def precedence(name: String): Int =
      if (customPrecedence) Term.Name(name).precedence else 0

    val outerOperatorIsLeftAssociative = isLeftAssociative(outerOperator)
    val innerOperatorIsLeftAssociative = isLeftAssociative(innerOperator)

    if (outerOperatorIsLeftAssociative ^ innerOperatorIsLeftAssociative) true
    else {
      val isLeft = outerOperatorIsLeftAssociative
      val isRight = !outerOperatorIsLeftAssociative

      val outerOperatorPrecedence = precedence(outerOperator)
      val innerOperatorPrecedence = precedence(innerOperator)

      if (outerOperatorPrecedence < innerOperatorPrecedence) isRight
      else if (outerOperatorPrecedence > innerOperatorPrecedence) isLeft
      else isLeft ^ side.isLeft
    }
  }

  private object NumericLiteral {
    def unapply(lit: Lit): Option[Unit] = {
      lit match {
        case _: Lit.Int | 
             _: Lit.Long |
             _: Lit.Double |
             _: Lit.Float |
             _: Lit.Byte |
             _: Lit.Short => Some(())
        case _ => None
      }
    }
  }

  private def groupNeedsParenthesis(
      outerGroup: SyntacticGroup,
      innerGroup: SyntacticGroup,
      side: Side
  ): Boolean = {
    import scala.meta.internal.fmt.{SyntacticGroup => groups}

    (outerGroup, innerGroup) match {
      case (groups.Term.InfixExpr(outerOperator), groups.Term.InfixExpr(innerOperator)) =>
        operatorNeedsParenthesis(
          outerOperator,
          innerOperator,
          customAssociativity = true,
          customPrecedence = true,
          side
        )
      case (groups.Type.InfixTyp(outerOperator), groups.Type.InfixTyp(innerOperator)) =>
        operatorNeedsParenthesis(
          outerOperator,
          innerOperator,
          customAssociativity = true,
          customPrecedence = false,
          side
        )
      case (groups.Pat.Pattern3(outerOperator), groups.Pat.Pattern3(innerOperator)) =>
        operatorNeedsParenthesis(
          outerOperator,
          innerOperator,
          customAssociativity = true,
          customPrecedence = true,
          side
        )
      case (groups.Term.PrefixExpr(outerOperator), groups.Path(Term.Select(NumericLiteral(_),_))) =>
        true
      case _ => {
        outerGroup.precedence > innerGroup.precedence
      }
    }
  }
}

object SyntacticGroup {
  sealed trait Type extends SyntacticGroup {
    def categories = List("Type")
  }
  object Type {
    case object ParamTyp extends Type { def precedence = 0 }
    case object Typ extends Type { def precedence = 1 }
    case object AnyInfixTyp extends Type { def precedence = 1.5 }
    case class InfixTyp(operator: String) extends Type { def precedence = 2 }
    case object RefineTyp extends Type { def precedence = 3 }
    case object WithTyp extends Type { def precedence = 3.5 }
    case object AnnotTyp extends Type { def precedence = 4 }
    case object SimpleTyp extends Type { def precedence = 6 }
  }
  sealed trait Term extends SyntacticGroup {
    def categories = List("Term")
  }
  object Term {
    case object Expr extends Term { def precedence = 0 }
    case object Expr1 extends Term { def precedence = 1 }
    case object Ascription extends Term { def precedence = 2 }
    case object PostfixExpr extends Term { def precedence = 2 }
    case class InfixExpr(operator: String) extends Term { def precedence = 3 }
    case class PrefixExpr(operator: String) extends Term { def precedence = 4 }
    case object SimpleExpr extends Term { def precedence = 5 }
    case object SimpleExpr1 extends Term { def precedence = 6 }
  }
  sealed trait Pat extends SyntacticGroup {
    def categories = List("Pat")
  }
  object Pat {
    case object Pattern extends Pat { def precedence = 0 }
    case object Pattern1 extends Pat { def precedence = 1 }
    case object Pattern2 extends Pat { def precedence = 2 }
    case object AnyPattern3 extends Pat { def precedence = 2.5 }
    case class Pattern3(operator: String) extends Pat { def precedence = 3 }
    case object SimplePattern extends Pat { def precedence = 6 }
  }
  case object Literal extends Term with Pat {
    override def categories = List("Term", "Pat"); def precedence = 6
  }
  require(
    Literal.precedence == Term.SimpleExpr1.precedence &&
      Literal.precedence == Pat.SimplePattern.precedence
  )
  case class Path(tree: Tree) extends Type with Term with Pat {
    override def categories = List("Type", "Term", "Pat");
    def precedence = 6
  }
  // require(
  //   Path.precedence == Type.SimpleTyp.precedence &&
  //     Path.precedence == Term.SimpleExpr1.precedence &&
  //     Path.precedence == Pat.SimplePattern.precedence
  // )

}

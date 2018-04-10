package org.scalafmt.tests
package tokens

import scala.meta._

import org.scalafmt.internal.tokens.SyntaxTokensTerm._

object SyntaxTokensTermSuite extends SyntaxTokensSuiteUtils {
  // checkSome[Term.Apply](_.tokensLeftParen)("f→(←a)")
  // checkSome[Term.Apply](_.tokensRightParen)("f(a→)←")
  // checkSome[Term.Apply](_.tokensLeftParen)("f→(←)")
  // checkSome[Term.Apply](_.tokensRightParen)("f(→)←")
  // checkSome[Term.Apply](_.tokensLeftBrace)("f →{← a }")
  // checkSome[Term.Apply](_.tokensRightBrace)("f { a →}←")

  checkNone[Term.ApplyInfix](_.tokensLeftParen)("x f y")
  checkNone[Term.ApplyInfix](_.tokensLeftParen)("x f g(y)")
  checkSome[Term.ApplyInfix](_.tokensLeftParen)("a f→(←b)")
  // checkSome[Term.ApplyInfix](_.tokensRightParen)("a f(b→)←")
  // checkSome[Term.ApplyInfix](_.tokensLeftBrace)("a f →{← b }")
  // checkSome[Term.ApplyInfix](_.tokensRightBrace)("a f { b →}←")
  // checkOne[Term.ApplyType](_.tokensLeftBracket)("f→[←T]")
  // checkOne[Term.ApplyType](_.tokensRightBracket)("f[T→]←")
  // checkOne[Term.Assign](_.tokensEqual)("x →=← 1")
  // checkOne[Term.Assign](_.tokensEqual)("x(a = 1) →=← 1")
  // checkOne[Term.Block](_.tokensLeftBrace)("→{←}")
  // checkOne[Term.Block](_.tokensRightBrace)("{→}←")
  // checkOne[Term.Block](_.tokensRightBrace)("{a; b→}←")
  // checkOne[Term.Do](_.tokensDo)("→do← d while (p)")
  // checkOne[Term.Do](_.tokensWhile)("do d →while← (p)")
  // checkOne[Term.Do](_.tokensLeftParen)("do d while →(←p)")
  // checkOne[Term.Do](_.tokensRightParen)("do d while (p→)←")
  // checkOne[Term.Eta](_.tokensUnderscore)("f →_←")
  // checkOne[Term.For](_.tokensFor)("→for← (a <- fa) ()")
  // checkOne[Term.ForYield](_.tokensFor)("→for← (a <- fa) yield a")
  // checkOne[Term.ForYield](_.tokensYield)("for (a <- fa) →yield← a")
  // checkOne[Term.Function](_.tokensRightArrow)("x →=>← 1")
  // checkOne[Term.If](_.tokensIf)("→if← (p) t")
  // checkOne[Term.If](_.tokensLeftParen)("if →(←(p)) t")
  // checkOne[Term.If](_.tokensRightParen)("if ((p)→)← t")
  // checkNone[Term.If](_.tokensElse)("if (p) t")
  // checkSome[Term.If](_.tokensElse)("if (p) t →else← f")
  // checkSome[Term.If](_.tokensElse)("if (p) { if (p2) t2 else f2 } →else← f")
  // checkOne[Term.Interpolate](_.tokensStartQuote)(s"s→$tq← a $dq b $dq c $tq")
  // checkOne[Term.Interpolate](_.tokensEndQuote)(s"s$tq a $dq b $dq c →$tq←")
  // checkOne[Term.Interpolate](_.tokensStartQuote)(s"s→$dq← a $dq")
  // checkOne[Term.Interpolate](_.tokensEndQuote)(s"s$dq a →$dq←")
  // checkOne[Term.PartialFunction](_.tokensLeftBrace)("→{← case _ => }")
  // checkOne[Term.PartialFunction](_.tokensRightBrace)("{ case _ => →}←")
  // checkOne[Term.Repeated, Term.Apply](_.tokensColon)("f(x→:← _*)")
  // checkOne[Term.Repeated, Term.Apply](_.tokensUnderscore)("f(x: →_←*)")
  // checkOne[Term.Repeated, Term.Apply](_.tokensAsterix)("f(x: _→*←)")
  // checkOne[Term.Return](_.tokensReturn)("→return← null")
  // checkSome[Term.Select](superDot)("a→.←super.b")
  // checkSome[Term.Select](superDot)("a→.←super[b].c")
  // checkNone[Term.Select](superDot)("super[a].b")
  // checkOne[Term.Select](_.tokensDot)("a→.←b")
  // checkNone[Term.This](_.tokensDot)("this")
  // checkSome[Term.This](_.tokensDot)("foo→.←this")
  // checkOne[Term.Throw](_.tokensThrow)("→throw← ex")
  // checkOne[Term.Ascribe](_.tokensColon)("a→:← A")
  // checkOne[Term.Annotate](_.tokensAt)("x: →@←u")
  // checkOne[Term.Tuple](_.tokensLeftParen)("→(←1, 1)")
  // checkOne[Term.Tuple](_.tokensRightParen)("(1, 1→)←")
  // checkOne[Term.Match](_.tokensMatch)("x →match← { case _ => }")
  // checkOne[Term.New](_.tokensNew)("→new← A")
  // checkOne[Term.NewAnonymous](_.tokensNew)("→new← A {}")
  // checkNone[Term.Param, Term.Function](_.tokenColon)("a => b")
  // checkSome[Term.Param, Decl.Def](_.tokenColon)("def f(x→:← A): B")
  // checkNone[Term.Param, Decl.Def](_.tokenEqual)("def f(x: A): B")
  // checkSome[Term.Param, Decl.Def](_.tokenEqual)("def f(x: A →=← 1): B")
  // checkOne[Term.Try](_.tokensTry)("→try← f catch { case _ => }")
  // checkNone[Term.Try](_.tokensLeftParen)("try f catch { case _ => }")
  // checkNone[Term.Try](_.tokensRightParen)("try f catch { case _ => }")
  // checkSome[Term.Try](_.tokensLeftParen)("try →(←f) catch { case _ => }")
  // checkSome[Term.Try](_.tokensRightParen)("try (f→)← catch { case _ => }")
  // checkSome[Term.Try](_.tokensRightParen)("try (f→)← finally { }")
  // checkSome[Term.Try](_.tokensCatch)("try (f) →catch← { case _ => }")
  // checkNone[Term.Try](_.tokensCatch)("try f finally {}")
  // checkNone[Term.Try](_.tokensFinally)("try f catch { case _ => }")
  // checkSome[Term.Try](_.tokensFinally)("try f →finally← {}")
  // checkOne[Term.TryWithHandler](_.tokensTry)("→try← f catch h")
  // checkNone[Term.TryWithHandler](_.tokensLeftParen)("try f catch h")
  // checkNone[Term.TryWithHandler](_.tokensRightParen)("try f catch h")
  // checkSome[Term.TryWithHandler](_.tokensLeftParen)("try →(←f) catch h")
  // checkSome[Term.TryWithHandler](_.tokensRightParen)("try (f→)← catch h")
  // checkOne[Term.TryWithHandler](_.tokensCatch)("try (f) →catch← h")
  // checkNone[Term.TryWithHandler](_.tokensFinally)("try f catch h")
  // checkSome[Term.TryWithHandler](_.tokensFinally)("try f catch h →finally← {}")
  // checkOne[Term.While](_.tokensWhile)("→while← (p) d")
  // checkOne[Term.While](_.tokensLeftParen)("while →(←p) d")
  // checkOne[Term.While](_.tokensRightParen)("while (p→)← d")
  // checkOne[Term.Placeholder](_.tokensUnderscore)("→_←")
  // →←
}

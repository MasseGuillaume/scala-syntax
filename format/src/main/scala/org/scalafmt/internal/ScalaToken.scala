package org.scalafmt.internal

import org.typelevel.paiges.Doc
import org.typelevel.paiges.Doc._
import org.typelevel.paiges.Doc.char
import org.typelevel.paiges.Doc.text

object ScalaToken {
  val backtick: Doc = char('`')
  val `@` : Doc = char('@')
  val `*` : Doc = char('*')
  val `.` : Doc = char('.')
  val `#` : Doc = char('#')
  val `(` : Doc = char('(')
  val `)` : Doc = char(')')
  val `[` : Doc = char('[')
  val `]` : Doc = char(']')
  val `{` : Doc = char('{')
  val `}` : Doc = char('}')
  val `:` : Doc = char(':')
  val `|` : Doc = char('|')
  val `&` : Doc = char('&')
  val `=` : Doc = char('=')
  val wildcard: Doc = char('_')
  val covariant: Doc = char('+')
  val contravariant: Doc = char('-')
  val `"` : Doc = char('"')
  val `$` : Doc = char('$')
  val `"""` : Doc = text("\"\"\"")
  val `super`: Doc = text("super")
  val `this`: Doc = text("this")
  val `=>` : Doc = text("=>")
  val `<-` : Doc = text("<-")
  val `type`: Doc = text("type")
  val `forSome`: Doc = text("forSome")
  val `throw`: Doc = text("throw")
  val `do`: Doc = text("do")
  val `while`: Doc = text("while")
  val `new`: Doc = text("new")
  val `override`: Doc = text("override")
  val `macro`: Doc = text("macro")
  val `lazy`: Doc = text("lazy")
  val `implicit`: Doc = text("implicit")
  val `abstract`: Doc = text("abstract")
  val `sealed`: Doc = text("sealed")
  val `case`: Doc = text("case")
  val `class`: Doc = text("class")
  val `def`: Doc = text("def")
  val `else`: Doc = text("else")
  val `extends`: Doc = text("extends")
  val `false`: Doc = text("false")
  val `final`: Doc = text("final")
  val `for`: Doc = text("for")
  val `yield`: Doc = text("yield")
  val `try`: Doc = text("try")
  val `catch`: Doc = text("catch")
  val `finally`: Doc = text("finally")
  val `if`: Doc = text("if")
  val `import`: Doc = text("import")
  val `match`: Doc = text("match")
  val `null`: Doc = text("null")
  val `return`: Doc = text("return")
  val `object`: Doc = text("object")
  val `package`: Doc = text("package")
  val `private`: Doc = text("private")
  val `protected`: Doc = text("protected")
  val `trait`: Doc = text("trait")
  val `true`: Doc = text("true")
  val `val`: Doc = text("val")
  val `var`: Doc = text("var")
  val `with`: Doc = text("with")
  val `>:` : Doc = text(">:")
  val `<:` : Doc = text("<:")
  val `<%` : Doc = text("<%")
  val lineBlank: Doc = lineNoFlatNoIndent + lineNoFlat
}

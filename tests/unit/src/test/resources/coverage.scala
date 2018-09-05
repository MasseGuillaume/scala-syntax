package a
package x.y {
  package z {}
}

package a {
  class A
}

class B

package object a

object Coverage {  
  null

  true
  
  false
  
  ()
  
  1
  
  0D
  
  1.0D
  
  4F
  
  1.0F
  
  1L

  'a'
  
  '\n'
  
  'a
  
  "hello"
  
  """a
     b"""


  for {
    `a` <- b    // Enumerator.Generator
    a <- b      // Enumerator.Generator
    a = 1       // Enumerator.Val
    c <- d if p // Enumerator.Guard
  }

  // meta.Case
  {
    case `a`             =>
    case `a` :: `b` :: _ =>
    case a b `c`         =>
    case _ op (a | b)    =>
    case x `.y` ()       =>
    case a if p          =>
    case _               => ()
    case _               => {}
  }

  // meta.Type
  // B                               // Type.Name
  // a.B                             // Type.Select
  // a#B                             // Type.Project
  // this.type                       // Type.Singleton
  // t.type                          // Type.Singleton
  // F[T]                            // Type.Apply
  // K Map V                         // Type.ApplyInfix
  // () => B                         // Type.Function
  // A => B                          // Type.Function
  // (A, B) => C                     // Type.Function
  // implicit A => B                 // Type.ImplicitFunction
  // (A, B)                          // Type.Tuple
  // A with B                        // Type.With
  // A & B                           // Type.And
  // // A | B                        // Type.Or (dotty)
  // A { def f: B }                  // Type.Refine
  // A{}                             // Type.Refine
  // { def f: B }                    // Type.Refine
  // A forSome { type T }            // Type.Existential
  // T @A                            // Type.Annotate
  // [X] => (X, X)                   // Type.Lambda
  // _                               // Type.Placeholder
  // _ >: A <: B                     // Type.Bounds
  // _ <: B                          // Type.Bounds (lower)
  // _ >: A                          // Type.Bounds (upper)
  // def f[A <% B[A]]: C             // Type.Bounds (view)
  // def f[A: B]: C                  // Type.Bounds (context)
  // def f[A : B : C]: D             // Type.Bounds (context)
  // => T                            // Type.ByName
  // Any*                            // Type.Repeated
  // trait A[X]                      // Type.Param
  // def f[@a A]: B                  // Type.Param (annotations)
  // List[t](xs @ _*                 // Type.Var

  // meta.Term
  this                       // Term.This
  a.this                     // Term.This
  a.super.b                  // Term.Super
  super[a].b                 // Term.Super
  a.super[b].c               // Term.Super
  a                          // Term.Name
  a.b                        // Term.Select
  -a                         // Term.ApplyUnary
  +a                         // Term.ApplyUnary
  ~a                         // Term.ApplyUnary
  !a                         // Term.ApplyUnary
  a(b)                       // Term.Apply
  a { case x => x }          // Term.Apply
  a { b }                    // Term.Apply
  a[B]                       // Term.ApplyType
  a + a                      // Term.ApplyInfix
  a + (())                   // Term.ApplyInfix
  (a, b) + c                 // Term.ApplyInfix
  a + (b, c)                 // Term.ApplyInfix
  a + ((b, c))               // Term.ApplyInfix
  a = 1                      // Term.Assign
  return b                   // Term.Return
  throw e                    // Term.Throw
  a: A                       // Term.Ascribe
  x: @u                      // Term.Annotate
  (1, 1)                     // Term.Tuple
  { a; b }                   // Term.Block
  if (p) t                   // Term.If
  if (p) {}                  // Term.If
  if (p) if (p2) t           // Term.If
  if (p) t else f            // Term.If
  x match { case _ => }      // Term.Match
  try f finally {}           // Term.Try
  try f catch { case _ => }  // Term.Try
  try f catch h              // Term.TryWithHandler
  try f catch h finally {}   // Term.TryWithHandler
  (a, b) => a + b            // Term.Function
  { case _ => }              // Term.PartialFunction
  while (p) d                // Term.While
  do d while (p)             // Term.Do
  for { x <- xs } f(x)       // Term.For
  for { x <- xs } yield f(x) // Term.ForYield
  (new A)                    // Term.New
  new A(1)                   // Term.New
  new A {}                   // Term.NewAnonymous
  _                          // Term.Placeholder
  f _                        // Term.Eta
  f(x: _*)                   // Term.Repeated
  def f(x: A): B             // Term.Param
  def f(x: A = 1): B         // Term.Param (default parameter)
  s"a $b"                    // Term.Interpolate
  <a>b {c}</a>               // Term.Xml

  

  // meta.{Import, Importee, Importer}
  import a.b
  import a.b, c.d
  import a._, c._
  import a._
  import a.{ b, c }
  import a.{ b => c }
  import a.{ b => _ }

  // meta.Self
  trait A { self: B => }
  trait A { _: B => }
  trait A { self => }
  trait A { this: B => }

  // meta.Template
  new A {}
  class A extends B with C with D
  class Y extends { val a = 1 } with X
  new { val a = 1 } with A {}

  // meta.Member
  
  // meta.Decl
  val a: Int      // Decl.Val
  var b: Long     // Decl.Var
  def f: String   // Decl.Def
  type T          // Decl.Type

  // meta.Defn
  val a = 1              // Defn.Val
  var a = 1              // Defn.Var
  var a: Int = _         // Defn.Var
  def a = 1              // Defn.Def
  def f = macro m        // Defn.Macro
  type T = Int           // Defn.Type
  class A                // Defn.Class
  class A(b: B)          // Defn.Class
  class A private (b: B) // Defn.Class
  trait A                // Defn.Trait
  object A               // Defn.Object

  // meta.Ctor
  class A { def this(a: A) = this() } // Ctor.Secondary
  
  class A {
    def this(a: A) = { // Ctor.Secondary
      this()
  
      f(b)
    }
  }

  // meta.Mod
  @tailrec def f = 1       // Mod.Annotation
  private[foo] val a = 1   // Mod.Private
  protected[foo] val a = 1 // Mod.Protected
  implicit val a = 1       // Mod.Implicit
  final val a = 1          // Mod.Final
  sealed trait a           // Mod.Sealed
  override def f = 1       // Mod.Override
  case object B            // Mod.Case
  abstract class A         // Mod.Abstract
  class A[+ T]             // Mod.Covariant
  class A[- T]             // Mod.Contravariant
  lazy val a = 1           // Mod.Lazy
  class A(val b: B)        // Mod.ValParam
  class A(var b: B)        // Mod.VarParam
  // inline def f = 1      // Mod.Inline (dotty)

  // meta.Pat
  {
    case `1` =>                      // Lit
    case a =>                        // Term.Name
    case a.b =>                      // Term.Select
    case a @ A =>                    // Pat.Var, Pat.Bind
    case _ =>                        // Pat.Wildcard
    case A(_*) =>                    // Pat.SeqWildcard
    case a | b =>                    // Pat.Alternative
    case (a, b) =>                   // Pat.Tuple
    case E(a, b) =>                  // Pat.Extract
    case a E b =>                    // Pat.ExtractInfix
    case r"example (.+)${foo}"=>     // Pat.Interpolate
    case <h1>a{b}c{d}e{f}g</h1> =>   // Pat.Xml
    case foo: Int =>                 // Pat.Typed
  }
}

}

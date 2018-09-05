/* L */ package A                       // T     Pkg
/* L */ package object a                // T     Pkg.Object
/* L */ import a.b                      // T     Import

object Comments {  
  /* L */ val a: Int                    // T    Decl.Val
  /* L */ var b: Long                   // T    Decl.Var
  /* L */ def f: String                 // T    Decl.Def
  /* L */ type S                        // T    Decl.Type

  /* L */ val a = 1                     // T    Defn.Val
  /* L */ var a = 1                     // T    Defn.Var
  /* L */ var a: Int = _                // T    Defn.Var
  /* L */ def a = 1                     // T    Defn.Def
  /* L */ def f = macro m               // T    Defn.Macro
  /* L */ type S = Int                  // T    Defn.Type
  /* L */ class A                       // T    Defn.Class
  /* L */ class A(b: B)                 // T    Defn.Class
  /* L */ class A private (b: B)        // T    Defn.Class
  /* L */ trait A                       // T    Defn.Trait
  /* L */ object A                      // T    Defn.Object

  // meta.Term
  // f /* L */{/* T */ x /* L */}/* T */
  // /* L */f/* T *//* L */(/* T *//* L */)/* T */

  f { x => x }                          // T
  f { case x => x }                  // T

  /* L */ a op f                        // T   Term.ApplyInfix
  // x f (g(x), y)                      // T   Term.ApplyInfix
  // x f (g(y))                         // T
  // x f (g(y))                         // T   Term.ApplyInfix
  // x f ((x, y))                       // T   Term.ApplyInfix

  // /* L */ (1, /* I */ 1)             // T   Term.Tuple
  // a/* L */./* T *//* L */this/* T */ // T   Term.This
  // a/* L */./* T */b                  // T   Term.Select
  /* L */ ! /* T */ a                   // T   Term.ApplyUnary

  // meta.Mod
  /* L */ @tailrec def f = 1            // T   Mod.Annotation
  /* L */ private[foo] val a = 1        // T   Mod.Private
  /* L */ protected[foo] val a = 1      // T   Mod.Protected
  /* L */ implicit val a = 1            // T   Mod.Implicit
  /* L */ final val a = 1               // T   Mod.Final
  /* L */ sealed trait a                // T   Mod.Sealed
  /* L */ override def f = 1            // T   Mod.Override
  /* L */ case object B                 // T   Mod.Case
  /* L */ abstract class A              // T   Mod.Abstract
  /* L */ lazy val a = 1                // T   Mod.Lazy

  class A[/* L */+T]               // Mod.Covariant
  class A[/* L */-T]               // Mod.Contravariant
  class A(/* L */val b: B)         // Mod.ValParam
  class A(/* L */var b: B)         // Mod.VarParam

  // == Advanced ==  
  {
    1 &                            // T
    2
  }

  (
    1 // T
    & 2
  )


  class C {
    /* L */ v // T
  }

  class C {
    v1 // T1
  
    v2 // T2
  }

  check(
    (1, // T
     2)
  )
}

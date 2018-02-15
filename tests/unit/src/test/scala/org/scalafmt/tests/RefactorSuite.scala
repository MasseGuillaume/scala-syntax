package org.scalafmt.tests

import scala.meta._
import scala.meta.internal.format.Comments._
import org.scalafmt.internal.TreePrinter

import utest._

trait Refactor {
  def name: String
  def apply(tree: Tree): Tree
}

trait RefactorStats extends Refactor {
  def applyStats(stats: List[Stat]): List[Stat]
  def apply(tree: Tree): Tree = {
    tree match {
      case Source(List(c: Defn.Class)) =>
        Source(List(c.copy(templ = c.templ.copy(stats = applyStats(c.templ.stats)))))
    }
  }
}

object RefactorSuite extends TestSuite {
  def test(refactor: Refactor, input: String, expected: String): Unit = {
    
    val obtained = TreePrinter.print(refactor(input.parse[Source].get)).render(100)
    println("== Obtained ==")
    println(obtained)
    println()
    println("== Expected ==")
    println(expected)
    println()
    // assert(obtained == expected)
  }

  def test(refactor: Refactor): Unit = {
    val path = refactor.name
    val input = slurpResource(s"/$path/input.scala")
    val expected = slurpResource(s"/$path/output.scala")
    test(refactor, input, expected)
  }

  val tests = Tests{
    object Move extends RefactorStats {
      def name: String = "move"
      def applyStats(stats: List[Stat]): List[Stat] = {
        val List(d1, d2) = stats
        List(d2, d1)
      }
    }

    'move {
      test(
        Move,
        "class A{val x = 1;val y = 1}",
        "class A{val y = 1;val x = 1}"
      )
    }


    object Remove extends RefactorStats {
      def name: String = "remove"
      def applyStats(stats: List[Stat]): List[Stat] = {
        val List(d: Defn.Def) = stats
        List(d.copy(decltpe = None))
      }
    }

    'remove {
      test(
        Remove,
        "class A{def foo(): Unit = ()}",
        "class A{def foo() = ()}"
      )
    }

    object Add extends RefactorStats {
      def name: String = "add"
      def applyStats(stats: List[Stat]): List[Stat] = {
        val List(d: Defn.Def) = stats
        List(d.copy(decltpe = Some(Type.Name("Unit"))))
      }
    }

    'add {
      test(
        Add,
        "class A{def foo() = ()}",
        "class A{def foo(): Unit = ()}"
      )
    }

    object Sam extends Refactor {
      def name: String = "sam"
      def unapply(tree: Tree): Option[(Term, Type)] = {
        tree match {
          case 
            Term.NewAnonymous(
              Template(
                _,
                List(Init(tpe, _, _)),
                _,
                List(
                  Defn.Def(
                    _,
                    _,
                    _,
                    List(params),
                    _,
                    body
                  )
                )
              )
            ) => {
              val lambda = Term.Function(params.map(_.copy(decltpe = None)), body)
              Some((lambda, tpe))
            }
          case _ => None
        }
      }

      def apply(in: Tree): Tree = {
        in.tree.transform {
          case term @ Defn.Val(_, _, None, Sam(lambda, tpe)) =>
            term.copy(decltpe = Some(tpe), rhs = lambda)
          case term @ Defn.Var(_, _, None, Sam(lambda, tpe)) =>
            term.copy(decltpe = Some(tpe), rhs = Some(lambda))
          case ap @ Term.Apply(_, args) =>
            var hasSam = false
            val argsWithSam =
              args.map{
                case Sam(lambda, _) => hasSam = true; lambda
                case m => m
              }

            if(hasSam) ap.copy(args = argsWithSam)
            else ap
        }
      }
    }

    'sam {
      test(
        Sam,
        """|object A {
           |  fun(new WithParams() {
           |    def doit(a: Int, b: Int): Int = {
           |      // comments
           |      a + b
           |    }
           |  })
           |}""".stripMargin,

        """|object A {
           |  fun(
           |    (a, b) => {
           |      // comments
           |      a + b
           |    }
           |  )
           |}""".stripMargin
      )
    }
  }

  def slurpResource(path: String): String = {
    import java.nio.charset.StandardCharsets
    import java.io.{BufferedInputStream, ByteArrayOutputStream}
    val bis = new BufferedInputStream(getClass.getResourceAsStream(path))
    val buf = new ByteArrayOutputStream()
    var result = bis.read()
    while(result != -1) {
      buf.write(result.toByte)
      result = bis.read()
    }
    buf.toString(StandardCharsets.UTF_8.name())
  }
}



package org.scalafmt.tests

import scala.meta._
import org.scalafmt.internal.{PreserveComments, PropertyResult}
import java.nio.charset.Charset

object CommentsSuite extends BaseScalaPrinterTest with PreserveComments {

  check("comments.scala")

  def check(path: String): Unit = {
    test(path) {
      val input = Input.Stream(getClass.getClassLoader.getResourceAsStream(path), Charset.forName("UTF-8"))
      check(input, path) match {
        case PropertyResult.Success => ()
        case PropertyResult.Failure(diff) => sys.error(diff) 
      }
    }
  }
}

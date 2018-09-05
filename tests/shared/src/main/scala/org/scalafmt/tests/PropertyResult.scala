package org.scalafmt.internal

sealed trait PropertyResult
object PropertyResult {
  case object Success extends PropertyResult
  case class Failure(explanation: String) extends PropertyResult
}

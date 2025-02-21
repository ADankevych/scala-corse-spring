package kse

object model:

  case class DummyError(message: String) extends Exception(message)

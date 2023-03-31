package be.vub.verifx

import scala.meta.inputs.{Input, Position}

sealed trait Error extends Throwable {
  val msg: String
  val pos: Position

  override def toString: String = {
    val line = pos.endLine + 1 // line and column numbers start from 0 but IDEs show line numbers starting from 1
    val column = pos.startColumn + 1
    val fileName = pos.input match {
      case f: Input.File => f.path.toString
      case f: Input.VirtualFile => f.path.toString
      case _ => ""
    }
    s"Error: $fileName (line $line, column $column) $msg"
  }
}

final case class UnknownIdentifierError(identifier: String, pos: Position) extends Error {
  val msg = s"Unknown identifier $identifier"
}

final case class IllegalOperationError(msg: String, pos: Position) extends Error

final case class ClassAlreadyExists(clazz: String, pos: Position) extends Error {
  val msg = s"Cannot redefine class $clazz"
}

final case class ClassNotFound(clazz: String, pos: Position) extends Error {
  val msg = s"Class $clazz does not exist."
}

final case class MethodNotFound(method: String, clazz: String, pos: Position) extends Error {
  val msg = s"$method is not a method of class $clazz"
}

final case class FieldDoesNotExist(clazz: String, field: String, pos: Position) extends Error {
  val msg = s"$field is not a field of class $clazz"
}

final case class ParseError(msg: String, pos: Position) extends Error

final case class TypeError(msg: String, pos: Position) extends Error

final case class MatchError(msg: String, pos: Position) extends Error

final case class IllegalArgumentException(msg: String, pos: Position) extends Error

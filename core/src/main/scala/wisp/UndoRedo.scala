package wisp

import scala.collection.mutable

/**
 * Created by rodneykinney on 4/17/15.
 */
class UndoRedo[T] {
  private val undoStack = new mutable.Stack[T]()
  private val redoStack = new mutable.Stack[T]()

  def head: Option[T] = undoStack.headOption

  def push(element: T) = ???

  def undo() = {
    if (undoStack.nonEmpty) {
      redoStack.push(undoStack.pop())
    }
    head
  }

  def redo() = {
    if (redoStack.nonEmpty) {
      undoStack.push(redoStack.pop())
    }
    head
  }
}

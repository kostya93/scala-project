package example

import example.CellType.CellType


case class GameState(smallFields: List[List[SmallField]] = List(SmallField() :: SmallField() :: SmallField() :: Nil,
  SmallField() :: SmallField() :: SmallField() :: Nil,
  SmallField() :: SmallField() :: SmallField() :: Nil),
                     smallWinners: SmallField = SmallField()) {}

case class SmallField(cells: List[List[Cell]] = List(Cell() :: Cell() :: Cell() :: Nil,
  Cell() :: Cell() :: Cell() :: Nil,
  Cell() :: Cell() :: Cell() :: Nil)) {}

case class Cell(var state: CellType = CellType.EMPTY) {}

object CellType extends Enumeration {
  type CellType = Value
  val CROSS, ZERO, EMPTY = Value
}

case class Point(x: Double, y: Double) {
  def toField(size: Int, padding: Int): Field = {
    var cX, cY, cx, cy: Int = -1
    val lineWidth = (size / 3 - 2 * padding) / 3
    if (x <= size / 3) {
      cX = 0
      if (x <= lineWidth + padding) {
        cx = 0
      } else if (x <= 2 * lineWidth + padding) {
        cx = 1
      } else {
        cx = 2
      }
    } else if (x <= 2 * size / 3) {
      cX = 1
      if (x <= size / 3 + lineWidth + padding) {
        cx = 0
      } else if (x <= size / 3 + 2 * lineWidth + padding) {
        cx = 1
      } else {
        cx = 2
      }
    } else {
      cX = 2
      if (x <= 2 * size / 3 + lineWidth + padding) {
        cx = 0
      } else if (x <= 2 * size / 3 + 2 * lineWidth + padding) {
        cx = 1
      } else {
        cx = 2
      }
    }

    if (y <= size / 3) {
      cY = 0
      if (y <= lineWidth + padding) {
        cy = 0
      } else if (y <= 2 * lineWidth + padding) {
        cy = 1
      } else {
        cy = 2
      }
    } else if (y <= 2 * size / 3) {
      cY = 1
      if (y <= size / 3 + lineWidth + padding) {
        cy = 0
      } else if (y <= size / 3 + 2 * lineWidth + padding) {
        cy = 1
      } else {
        cy = 2
      }
    } else {
      cY = 2
      if (y <= 2 * size / 3 + lineWidth + padding) {
        cy = 0
      } else if (y <= 2 * size / 3 + 2 * lineWidth + padding) {
        cy = 1
      } else {
        cy = 2
      }
    }
    Field(cX, cY, cx, cy)
  }
}

case class Field(X: Int, Y: Int, x: Int, y: Int) {
}

package example

import example.CellType.CellType
import example.PlayerSide.PlayerSide

case class Game(var state: GameState = GameState(), var turn: PlayerSide = PlayerSide.CROSS, var lastMove: Field = null) {
  def reset(): Unit = {
    state = GameState()
    turn = PlayerSide.CROSS
    lastMove = null
  }

  private def isSmallFieldFull(field: SmallField): Boolean = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (field.cells(i)(j).state == CellType.EMPTY) {
          return false
        }
      }
    }
    true
  }

  def move(field: Field, player: PlayerSide): Boolean = {
    if (checkWin()) {
      return false
    }

    if (player != turn) {
      return false
    }

    if (state.smallFields(field.X)(field.Y).cells(field.x)(field.y).state != CellType.EMPTY) {
      return false
    }

    if (lastMove != null
      && (lastMove.x != field.X || lastMove.y != field.Y)
      && !isSmallFieldFull(state.smallFields(lastMove.x)(lastMove.y))) {
      return false
    }

    _move(field, player)
    true
  }

  def updateSmallWinners() = {
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (state.smallWinners.cells(i)(j).state == CellType.EMPTY) {
          state.smallWinners.cells(i)(j).state = playerSide2cellType(_checkWin(state.smallFields(i)(j)))
        }
      }
    }
  }

  private def _move(field: Field, player: PlayerSide): Unit = {
    state.smallFields(field.X)(field.Y).cells(field.x)(field.y).state = playerSide2cellType(player)
    lastMove = field
    updateSmallWinners()
    turn match {
      case PlayerSide.CROSS => turn = PlayerSide.ZERO
      case PlayerSide.ZERO => turn = PlayerSide.CROSS
    }
  }

  def isEnded: Boolean = {
    if (checkWin()) {
      return true
    }

    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        if (!isSmallFieldFull(state.smallFields(i)(j))) {
          return false
        }
      }
    }

    true
  }

  def checkWin(): Boolean = {
    _checkWin(state.smallWinners) != null
  }

  private def _checkWin(smallField: SmallField): PlayerSide = {
    for (i <- 0 until 3) {
      if (
        smallField.cells(i)(0).state == smallField.cells(i)(1).state &&
          smallField.cells(i)(1).state == smallField.cells(i)(2).state &&
          smallField.cells(i)(2).state != CellType.EMPTY
      ) {
        return cellType2playerSide(smallField.cells(i)(2).state)
      }

      if (
        smallField.cells(0)(i).state == smallField.cells(1)(i).state &&
          smallField.cells(1)(i).state == smallField.cells(2)(i).state &&
          smallField.cells(2)(i).state != CellType.EMPTY
      ) {
        return cellType2playerSide(smallField.cells(2)(i).state)
      }
    }

    if (
      smallField.cells(0)(0).state == smallField.cells(1)(1).state &&
        smallField.cells(1)(1).state == smallField.cells(2)(2).state &&
        smallField.cells(2)(2).state != CellType.EMPTY
    ) {
      return cellType2playerSide(smallField.cells(2)(2).state)
    }

    if (
      smallField.cells(0)(2).state == smallField.cells(1)(1).state &&
        smallField.cells(1)(1).state == smallField.cells(2)(0).state &&
        smallField.cells(2)(0).state != CellType.EMPTY
    ) {
      return cellType2playerSide(smallField.cells(2)(0).state)
    }

    null
  }

  def playerSide2cellType(playerSide: PlayerSide): CellType = {
    playerSide match {
      case PlayerSide.CROSS => CellType.CROSS
      case PlayerSide.ZERO => CellType.ZERO
      case _ => CellType.EMPTY
    }
  }

  def cellType2playerSide(cellType: CellType): PlayerSide = {
    cellType match {
      case CellType.CROSS => PlayerSide.CROSS
      case CellType.ZERO => PlayerSide.ZERO
      case CellType.EMPTY => null
    }
  }
}

object PlayerSide extends Enumeration {
  type PlayerSide = Value
  val CROSS, ZERO = Value
}

package example

import example.PlayerSide.PlayerSide

/**
  * Created by kostya on 19.12.2016.
  */
trait Strategy {
  def move(game: Game, playerSide: PlayerSide): Boolean
}

case class StrategyRandomImpl() extends Strategy {
  override def move(game: Game, playerSide: PlayerSide): Boolean = {
    val random = scala.util.Random
    var x = random.nextInt(3)
    var y = random.nextInt(3)
    var X = random.nextInt(3)
    var Y = random.nextInt(3)
    if (game.turn != playerSide) {
      return false
    }
    if (game.checkWin()) {
      return false
    }
    while (!game.move(Field(X, Y, x, y), playerSide)) {
      x = random.nextInt(3)
      y = random.nextInt(3)
      X = random.nextInt(3)
      Y = random.nextInt(3)
    }
    true
  }
}
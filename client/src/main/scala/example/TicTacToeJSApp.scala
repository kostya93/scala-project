package example

import org.scalajs.dom
import org.scalajs.dom.ext.Ajax
import org.scalajs.dom.html
import org.scalajs.dom.html._

import scala.scalajs.js.JSApp

object TicTacToeJSApp extends JSApp {
  private var userSide = PlayerSide.CROSS
  private val game = Game()
  private val ai = StrategyRandomImpl()
  var gameStarted = false
  var point: Point = Point(0, 0)
  val canvasPainter = CanvasPainter(dom.document.getElementById("canvas").asInstanceOf[Canvas], processMove)
  canvasPainter.draw(game)

  def main(): Unit = {
    val newGameBtn = dom.document.getElementById("new-game").asInstanceOf[html.Input]
    setupNewGameBtn(newGameBtn)
  }

  def processMove(point: Point): Unit = {
    if (!gameStarted) {
      return
    }
    val filed = point.toField(canvasPainter.size, canvasPainter.padding)
    dom.window.console.log(s"filed $filed")
    if (game.move(filed, userSide)) {
      canvasPainter.draw(game)
      if (game.checkWin()) {
        processWin()
        return
      }
      if (game.isEnded) {
        processDraw()
        return
      }
      ai.move(game, PlayerSide.ZERO)
      canvasPainter.draw(game)
      if (game.checkWin()) {
        processLose()
        return
      }
      if (game.isEnded) {
        processDraw()
        return
      }
    }
  }

  def setupNewGameBtn(newGameBtn: Input): Unit = {
    newGameBtn.onclick = {
      (e: dom.MouseEvent) => {
        Ajax.post("/new-game", withCredentials = true)
        gameStarted = true
        canvasPainter.showCanvas()
        userSide = PlayerSide.CROSS
        game.reset()
        canvasPainter.draw(game)
      }
    }
  }

  def processWin() = {
    Ajax.post("/win-game", withCredentials = true)
    dom.window.alert("You win")
  }

  def processLose() = {
    dom.window.alert("You lose")
  }

  def processDraw() = {
    Ajax.post("/draw-game", withCredentials = true)
    dom.window.alert("draw")
  }
}

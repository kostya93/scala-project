package example

import example.CellType.CellType
import org.scalajs.dom

/**
  * Created by kostya on 19.12.2016.
  */

case class CanvasPainter(private val canvas: dom.html.Canvas, callback: (Point) => Unit, size: Int = 600, padding: Int = 10) {
  private val COLOR_CROSS = "#6098f2"
  private val COLOR_ZERO = "#f26060"
  private val COLOR_BACKGROUND = "#fcfcfc"
  private val COLOR_SMALL_FIELD_LINES = "#000000"
  private val COLOR_BIG_FIELD_LINES = "#e8e8e8"
  private val COLOR_LAST_MOVE = "#b6d8b6"
  private val COLOR_SMALL_WINNER_CROSS = "#eaf2ff"
  private val COLOR_SMALL_WINNER_ZERO = "#ffeaea"

  canvas.width = size
  canvas.height = size
  canvas.style.display = "none"

  private val renderer = canvas.getContext("2d").asInstanceOf[dom.CanvasRenderingContext2D]
  canvas.onmousedown = {
    (e: dom.MouseEvent) =>
      val rect = canvas.getBoundingClientRect()
      val point = Point(e.clientX - rect.left, e.clientY - rect.top)
      dom.window.console.log(s"$point")
      callback(point)
  }

  def clearCanvas() = {
    dom.window.console.log("clearCanvas")
    renderer.fillStyle = COLOR_BACKGROUND
    renderer.fillRect(0, 0, canvas.width, canvas.height)
  }

  def drawSmallGrids() = {
    dom.window.console.log("drawSmallGrids")
    val firstLine = size / 3 + padding
    val secondLine = 2 * (size / 3) + padding

    drawSmallField(padding, padding)
    drawSmallField(firstLine, padding)
    drawSmallField(secondLine, padding)

    drawSmallField(padding, firstLine)
    drawSmallField(firstLine, firstLine)
    drawSmallField(secondLine, firstLine)

    drawSmallField(padding, secondLine)
    drawSmallField(firstLine, secondLine)
    drawSmallField(secondLine, secondLine)
  }

  def draw(game: Game): Unit = {
    dom.window.console.log("draw")
    clearCanvas()
    drawSmallWinners(game.state.smallWinners)
    drawBigGrid()
    drawSmallGrids()
    for (cX <- 0 until 3) {
      for (cY <- 0 until 3) {
        for (x <- 0 until 3) {
          for (y <- 0 until 3) {
            val cellState = game.state.smallFields(cX)(cY).cells(x)(y).state
            drawCell(cellState, Field(cX, cY, x, y))
          }
        }
      }
    }
    if (game.lastMove != null) {
      drawLastMove(game.lastMove)
    }
  }

  def drawSmallWinner(X: Int, Y: Int, cell: Cell): Unit = {
    dom.window.console.log("drawSmallWinner")
    var color: String = null
    cell.state match {
      case CellType.CROSS => color = COLOR_SMALL_WINNER_CROSS
      case CellType.ZERO => color = COLOR_SMALL_WINNER_ZERO
      case _ => return
    }
    dom.window.console.log(s"drawSmallWinner = $color")

    val bigLine = size / 3
    renderer.fillStyle = color
    renderer.fillRect(X * bigLine, Y * bigLine, bigLine, bigLine)
  }

  def drawSmallWinners(smallWinners: SmallField) = {
    dom.window.console.log(s"$smallWinners")
    dom.window.console.log("drawSmallWinners")
    for (i <- 0 until 3) {
      for (j <- 0 until 3) {
        drawSmallWinner(i, j, smallWinners.cells(i)(j))
      }
    }
  }

  def drawLastMove(lastMove: Field) = {
    dom.window.console.log("drawLastMove")
    renderer.beginPath()

    val bigLine = size / 3
    val smallLine = (size / 3 - 2 * padding) / 3
    val smallPadding = padding / 2

    val left = lastMove.X * bigLine + lastMove.x * smallLine + smallPadding + padding
    val right = left + (smallLine - 2 * smallPadding)
    val top = lastMove.Y * bigLine + lastMove.y * smallLine + smallPadding + padding
    val bottom = top + (smallLine - 2 * smallPadding)

    renderer.moveTo(left, top)
    renderer.lineTo(right, top)
    renderer.lineTo(right, bottom)
    renderer.lineTo(left, bottom)
    renderer.lineTo(left, top)

    renderer.strokeStyle = COLOR_LAST_MOVE
    renderer.lineWidth = 2
    renderer.stroke()
  }

  def drawCell(cellType: CellType, field: Field) = {
    cellType match {
      case CellType.CROSS => drawCross(field)
      case CellType.ZERO => drawZero(field)
      case _ =>
    }
  }

  def drawZero(field: Field): Unit = {
    renderer.beginPath()

    val bigLine = size / 3
    val smallLine = (size / 3 - 2 * padding) / 3
    val smallPadding = padding

    val left = field.X * bigLine + field.x * smallLine + smallPadding + padding
    val right = left + (smallLine - 2 * smallPadding)
    val top = field.Y * bigLine + field.y * smallLine + smallPadding + padding
    val bottom = top + (smallLine - 2 * smallPadding)

    val centerX = (right + left) / 2
    val centerY = (bottom + top) / 2
    val radius = (right - left) / 2

    renderer.arc(centerX, centerY, radius, 0, 2 * Math.PI)

    renderer.strokeStyle = COLOR_ZERO
    renderer.lineWidth = 2
    renderer.stroke()
  }

  def drawCross(field: Field): Unit = {
    renderer.beginPath()

    val bigLine = size / 3
    val smallLine = (size / 3 - 2 * padding) / 3
    val smallPadding = padding

    val left = field.X * bigLine + field.x * smallLine + smallPadding + padding
    val right = left + (smallLine - 2 * smallPadding)
    val top = field.Y * bigLine + field.y * smallLine + smallPadding + padding
    val bottom = top + (smallLine - 2 * smallPadding)

    renderer.moveTo(left, top)
    renderer.lineTo(right, bottom)
    renderer.moveTo(right, top)
    renderer.lineTo(left, bottom)

    renderer.strokeStyle = COLOR_CROSS
    renderer.lineWidth = 2
    renderer.stroke()
  }

  private def drawBigGrid() = {
    dom.window.console.log("drawBigGrid")
    renderer.beginPath()
    renderer.moveTo(size / 3, 0)
    renderer.lineTo(size / 3, size)
    renderer.moveTo(2 * size / 3, 0)
    renderer.lineTo(2 * size / 3, size)

    renderer.moveTo(0, size / 3)
    renderer.lineTo(size, size / 3)
    renderer.moveTo(0, 2 * size / 3)
    renderer.lineTo(size, 2 * size / 3)

    renderer.strokeStyle = COLOR_BIG_FIELD_LINES
    renderer.lineWidth = 1
    renderer.stroke()
  }

  private def drawSmallField(x: Double, y: Double): Unit = {
    renderer.beginPath()

    val width = size / 3 - 2 * padding
    val firstLine = width / 3
    val secondLine = 2 * (width / 3)

    renderer.moveTo(x + firstLine, y)
    renderer.lineTo(x + firstLine, y + width)
    renderer.moveTo(x + secondLine, y)
    renderer.lineTo(x + secondLine, y + width)

    renderer.moveTo(x, y + firstLine)
    renderer.lineTo(x + width, y + firstLine)
    renderer.moveTo(x, y + secondLine)
    renderer.lineTo(x + width, y + secondLine)

    renderer.strokeStyle = COLOR_SMALL_FIELD_LINES
    renderer.lineWidth = 1
    renderer.stroke()
  }

  def showCanvas(): Unit = {
    canvas.style.display = "block"
  }
}

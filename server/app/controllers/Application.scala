package controllers

import models.Data
import play.api.mvc._

class Application extends Controller with Secured {
  def index = withUser {
    user => implicit request => Ok(views.html.index(user))
  }

  def newGame = withUser {
    user => implicit request => {
      Data.changeScore(user, -25)
      Ok
    }
  }

  def winGame = withUser {
    user => implicit request => {
      Data.changeScore(user, 50)
      Ok
    }
  }

  def drawDame = withUser {
    user => implicit request => {
      Data.changeScore(user, 25)
      Ok
    }
  }

  def leaderboard = Action {
    Ok(views.html.leaderboard(Data.sorted().toList))
  }
}

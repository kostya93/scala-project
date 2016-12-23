package controllers

import models.Data
import play.api.data.Forms._
import play.api.data._
import play.api.mvc._
import views._

/**
  * Created by kostya on 20.12.2016.
  */

class Auth extends Controller {


  val signupForm: Form[(String, String)] = Form(
    tuple(
      "username" -> text,
      "password" -> text
    ) verifying("User already exist", result => result match {
      case (username, password) => checkSingup(username: String, password: String)
    })
  )

  val loginForm: Form[(String, String)] = Form(
    tuple(
      "username" -> text,
      "password" -> text
    ) verifying("Invalid username or password", result => result match {
      case (username, password) => checkLogin(username: String, password: String)
    })
  )

  def checkLogin(username: String, password: String): Boolean = {
    Data.check(username, password)
  }

  def checkSingup(username: String, password: String): Boolean = {
    Data.findByUserName(username).isEmpty
  }

  def login = Action { implicit request =>
    Ok(html.login(loginForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.login(formWithErrors)),
      user => Redirect(routes.Application.index()).withSession(Security.username -> user._1)
    )
  }

  def register = Action { implicit request =>
    signupForm.bindFromRequest.fold(
      formWithErrors => BadRequest(html.signup(formWithErrors)),
      user => {
        Data.addUser(user._1, user._2)
        Redirect(routes.Application.index()).withSession(Security.username -> user._1)
      }
    )
  }

  def signup = Action {
    Ok(html.signup(signupForm))
  }

  def logout = Action {
    Redirect(routes.Auth.login()).withNewSession.flashing(
      "success" -> "You are now logged out."
    )
  }
}

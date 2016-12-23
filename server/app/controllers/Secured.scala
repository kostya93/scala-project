package controllers

import models.Data
import play.api.mvc._
import shared.User

/**
  * Created by kostya on 20.12.2016.
  */
trait Secured {

  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnauthorized(request: RequestHeader) = Results.Redirect(routes.Auth.login())

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnauthorized) { user =>
      Action(request => f(user)(request))
    }
  }

  def withUser(f: User => Request[AnyContent] => Result) = withAuth { username => implicit request =>
    Data.findByUserName(username).map { user =>
      f(user)(request)
    }.getOrElse(onUnauthorized(request))
  }
}

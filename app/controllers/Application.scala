package controllers

import play.api._
import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

// Authorization
trait Secured {
  def username(request: RequestHeader) = request.session.get(Security.username)

  def onUnAuthorized(request: RequestHeader) = Results.Redirect(routes.Application.login)

  def withAuth(f: => String => Request[AnyContent] => Result) = {
    Security.Authenticated(username, onUnAuthorized) { user =>
      Action(request => f(user)(request))
    }
  }
}

// controller

object Application extends Controller with Secured {

  def index = withAuth { username =>
    implicit request =>
      Ok(views.html.index(username))
  }

  val loginForm = Form(
    tuple(
      "username" -> text,
      "password" -> text) verifying ("Invalid email or password", result => result match {
        case (username, password) => check(username, password)
      }))

  def check(username: String, password: String) = {
    (username == "admin" && password == "1234")
  }

  def login = Action {
    implicit request =>
      Ok(views.html.login(loginForm))
  }

  def authenticate = Action { implicit request =>
    loginForm.bindFromRequest.fold(
      formWithErrors => BadRequest(views.html.login(formWithErrors)),
      user => {
        play.Logger.debug(" *** GREAT SUCCESS *** ")
        Redirect(routes.Application.index).withSession(Security.username -> user._1)
      })
  }

  def logout = Action {
    Redirect(routes.Application.login).withNewSession.flashing(
      "success" -> "You are logged out.")
  }

}

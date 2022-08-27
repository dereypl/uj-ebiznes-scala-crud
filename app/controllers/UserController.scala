package controllers

import model.{NewUser, User}

import javax.inject._
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import scala.collection.mutable


@Singleton
class UserController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  private val usersList = new mutable.ListBuffer[User]()
  usersList += User(1, "Mateusz")
  usersList += User(2, "Mariusz")

  implicit val userJson = Json.format[User]
  implicit val newUserJson = Json.format[NewUser]

  def getAll(): Action[AnyContent] = Action {
    if (usersList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(usersList))
    }
  }

  def getById(id: Long) = Action {
    val foundItem = usersList.find(_.id == id)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def update(id: Long) = Action { implicit request =>
    val requestName = request.body.asJson.get
    val foundItem = usersList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(name = (requestName \ "name").as[String])
        usersList.dropWhileInPlace(_.id == id)
        usersList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  def delete(id: Long) = Action {
    val userToDelete: Option[User] = usersList.find(user => user.id == id)
    userToDelete match {
      case Some(u) =>
        usersList.remove(usersList.indexOf(u))
        Accepted
      case None =>
        BadRequest
    }

  }

  def add() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val users: Option[NewUser] = jsonObject.flatMap(Json.fromJson[NewUser](_).asOpt)

    users match {
      case Some(newItem) =>
        val nextId = usersList.map(_.id).max + 1
        val created = User(nextId, newItem.name)
        usersList += created
        Created(Json.toJson(created))
      case None =>
        BadRequest
    }
  }
}

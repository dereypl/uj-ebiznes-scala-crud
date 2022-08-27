package controllers

import model.{NewCategory, Category}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject._
import scala.collection.mutable


@Singleton
class CategoryController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  private val categoriesList = new mutable.ListBuffer[Category]()
  categoriesList += Category(1, "Telefony komórkowe")
  categoriesList += Category(2, "Monitory")

  implicit val categoryJson = Json.format[Category]
  implicit val newCategoryJson = Json.format[NewCategory]

  def getAll(): Action[AnyContent] = Action {
    if (categoriesList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(categoriesList))
    }
  }

  def getById(id: Long) = Action {
    val foundItem = categoriesList.find(_.id == id)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def update(id: Long) = Action { implicit request =>
    val requestBody = request.body.asJson.get
    val foundItem = categoriesList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(name = (requestBody \ "name").as[String])
        categoriesList.dropWhileInPlace(_.id == id)
        categoriesList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  def delete(id: Long) = Action {
    val categoryToDelete: Option[Category] = categoriesList.find(category => category.id == id)
    categoryToDelete match {
      case Some(u) =>
        categoriesList.remove(categoriesList.indexOf(u))
        Accepted
      case None =>
        BadRequest
    }

  }

  def add() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val categories: Option[NewCategory] = jsonObject.flatMap(Json.fromJson[NewCategory](_).asOpt)

    categories match {
      case Some(newItem) =>
        val nextId = categoriesList.map(_.id).max + 1
        val created = Category(nextId, newItem.name)
        categoriesList += created
        Created(Json.toJson(created))
      case None =>
        BadRequest
    }
  }
}
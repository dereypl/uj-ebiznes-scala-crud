package controllers

import model.{NewProduct, Product}
import play.api.libs.json._
import play.api.mvc.{Action, AnyContent, BaseController, ControllerComponents}

import javax.inject._
import scala.collection.mutable


@Singleton
class ProductController @Inject()(val controllerComponents: ControllerComponents)
  extends BaseController {
  private val productsList = new mutable.ListBuffer[Product]()
  productsList += Product(1, 1, "OnePlus 10T 5G 16GB/256GB Moonstone Black", 4199.00)
  productsList += Product(2, 2, "Acer EK240YCbi", 539.99)

  implicit val productJson = Json.format[Product]
  implicit val newProductJson = Json.format[NewProduct]

  def getAll(): Action[AnyContent] = Action {
    if (productsList.isEmpty) {
      NoContent
    } else {
      Ok(Json.toJson(productsList))
    }
  }

  def getById(id: Long) = Action {
    val foundItem = productsList.find(_.id == id)
    foundItem match {
      case Some(item) => Ok(Json.toJson(item))
      case None => NotFound
    }
  }

  def update(id: Long) = Action { implicit request =>
    val requestBody = request.body.asJson.get
    val foundItem = productsList.find(_.id == id)
    foundItem match {
      case Some(item) =>
        val newItem = item.copy(name = (requestBody \ "name").as[String], categoryId = (requestBody \ "categoryId").as[Long], price = (requestBody \ "price").as[Double])
        productsList.dropWhileInPlace(_.id == id)
        productsList += newItem
        Accepted(Json.toJson(newItem))
      case None => NotFound
    }
  }

  def delete(id: Long) = Action {
    val productToDelete: Option[Product] = productsList.find(product => product.id == id)
    productToDelete match {
      case Some(u) =>
        productsList.remove(productsList.indexOf(u))
        Accepted
      case None =>
        BadRequest
    }

  }

  def add() = Action { implicit request =>
    val content = request.body
    val jsonObject = content.asJson

    val products: Option[NewProduct] = jsonObject.flatMap(Json.fromJson[NewProduct](_).asOpt)

    products match {
      case Some(newItem) =>
        val nextId = productsList.map(_.id).max + 1
        val created = Product(nextId, newItem.categoryId, newItem.name, newItem.price)
        productsList += created
        Created(Json.toJson(created))
      case None =>
        BadRequest
    }
  }
}
package model

case class Product(id: Long, categoryId: Long, name: String, price: Double)

case class NewProduct(name: String, categoryId: Long, price: Double)
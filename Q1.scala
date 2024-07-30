case class Product(name: String, quantity: Int, price: Double)

// Maps for Inventories
var inventory1: Map[Int, Product] = Map(
  101 -> Product("Product 01", 10, 100.0),
  102 -> Product("Product 02", 5, 150.0),
  103 -> Product("Product 03", 20, 200.0)
)

var inventory2: Map[Int, Product] = Map(
  102 -> Product("Product 02", 10, 180.0),
  104 -> Product("Product 04", 7, 120.0)
)

@main
def main(): Unit = {

  println("Products in Inventory 1 : " + getProductNames(inventory1))
  println("Total Value of Inventory 1 : " + calculateTotalValue(inventory1))
  println("Inventory 1 Status : " + isInventoryEmpty(inventory1))
  println("Merged Inventory : " + mergeInventories(inventory1, inventory2))

  checkProductExists(inventory1, 102)
  checkProductExists(inventory1, 105)
}

// I. Retrieve all product names from inventory1
def getProductNames(inventory: Map[Int, Product]): List[String] = {
  inventory.values.map(_.name).toList
}

// II. Calculate the total value of all products in inventory1
def calculateTotalValue(inventory: Map[Int, Product]): Double = {
  inventory.values.map(product => product.quantity * product.price).sum
}

// III. Check if inventory1 is empty
def isInventoryEmpty(inventory: Map[Int, Product]): String = {
  if (inventory.isEmpty) "Empty" else "Not Empty"
}

// IV. Merge inventory1 and inventory2, updating quantities and retaining the highest price
def mergeInventories(
    inv1: Map[Int, Product],
    inv2: Map[Int, Product]
): Map[Int, Product] = {
  inv1 ++ inv2.map { case (id, product2) =>
    id -> inv1
      .get(id)
      .map { product1 =>
        Product(
          product1.name,
          product1.quantity + product2.quantity,
          Math.max(product1.price, product2.price)
        )
      }
      .getOrElse(product2)
  }
}

// V. Check if a product with a specific ID exists and print its details
def checkProductExists(inventory: Map[Int, Product], id: Int): Unit = {
  inventory.get(id) match {
    case Some(product) =>
      println(
        s"Product ID: $id, Name: ${product.name}, Quantity: ${product.quantity}, Price: ${product.price}"
      )
    case None => println(s"Product with ID $id does not exist.")
  }
}

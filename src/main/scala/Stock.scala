import java.io._
import scala.collection.immutable.List

object Stock {

  case class Client(name: String, Money: Int, A: Int, B: Int, C: Int, D: Int)
  case class Order(name: String, orderType: String, stockType: String, price: Int, quantity: Int, rn: Int)

  private def convertClients(clients: scala.collection.mutable.Map[String, Client]): List[List[String]] = {
    val clientsList: List[List[String]] = clients.values.toList.map { client =>
      List(
        client.name,
        client.Money.toString,
        client.A.toString,
        client.B.toString,
        client.C.toString,
        client.D.toString
      )
    }
    clientsList
  }

  def writeResult(filePath: String, clients: List[List[String]]): Unit = {
    val writer = new PrintWriter(new File(filePath))
    try {
      clients.foreach { client =>
        writer.println(s"${client(0)}\t${client(1)}\t${client(2)}\t${client(3)}\t${client(4)}\t${client(5)}")
      }
    } finally {
      writer.close()
    }
  }

  private def checkSeller(seller: Client, stock: String, quantity: Int): Boolean = {
    val clientQuantity = stock match {
      case "A" => seller.A
      case "B" => seller.B
      case "C" => seller.C
      case "D" => seller.D
    }
    clientQuantity >= quantity
  }
  private def checkBuyer(buyer: Client, pay: Int): Boolean = {
    buyer.Money >= pay
  }

  // (C1, 1000, 130, 240, 760, 320) case class Client(name: String, Money: Int, A: Int, B: Int, C: Int, D: Int)
  private def updateClient(clients: scala.collection.mutable.Map[String, Client], clientName: String, stock: String, price: Int, quantity: Int): Unit = {
    val client = clients(clientName)
    val updatedClient = stock match {
      case "A" => client.copy(Money = client.Money + price * quantity, A = client.A + quantity)
      case "B" => client.copy(Money = client.Money + price * quantity, B = client.B + quantity)
      case "C" => client.copy(Money = client.Money + price * quantity, C = client.C + quantity)
      case "D" => client.copy(Money = client.Money + price * quantity, D = client.D + quantity)
    }
    clients(clientName) = updatedClient
  }

  // -----------------------------------------------------------
  private def matchOrders(orders: List[Order], clients: scala.collection.mutable.Map[String, Client]) = {
    val buyOrders = scala.collection.mutable.Map[String, scala.collection.mutable.Set[Order]]()
    val sellOrders = scala.collection.mutable.Map[String, scala.collection.mutable.Set[Order]]()

    for (order <- orders) {
      val key = List(order.stockType, order.price, order.quantity).mkString("|")
      //Array(C8, b, C, 15, 4) case class Order(name: String, orderType: String, stockType: String, price: Int, quantity: Int)
      order.orderType match {
        case "b" =>
          sellOrders.get(key) match {
            case Some(sellSet) =>
              var matched = false
              for (sellOrder <- sellSet if !matched) {
                if ((order.name != sellOrder.name) && (checkSeller(clients(sellOrder.name), sellOrder.stockType, sellOrder.quantity) && checkBuyer(clients(order.name), order.price * order.quantity))) {
                  matched = true
                  sellSet.remove(sellOrder)
                  updateClient(clients, order.name, order.stockType, -order.price, order.quantity)
                  updateClient(clients, sellOrder.name, sellOrder.stockType, -sellOrder.price, -sellOrder.quantity)
                }
              }
            case None =>
              buyOrders.getOrElseUpdate(key, scala.collection.mutable.Set[Order]()) += order
          }
        case "s" =>
          buyOrders.get(key) match {
            case Some(buySet) =>
              var matched = false
              for (buyOrder <- buySet if !matched) {
                if ((order.name != buyOrder.name) && (checkSeller(clients(order.name), order.stockType, order.quantity) && checkBuyer(clients(buyOrder.name), buyOrder.price * buyOrder.quantity))) {
                  matched = true
                  buySet.remove(buyOrder)
                  updateClient(clients, buyOrder.name, buyOrder.stockType, -buyOrder.price, buyOrder.quantity)
                  updateClient(clients, order.name, order.stockType, -order.price, -order.quantity)
                }
              }
            case None =>
              sellOrders.getOrElseUpdate(key, scala.collection.mutable.Set[Order]()) += order
          }
      }
    }
    clients.foreach { case (name, client) =>
      println(s"Client $name: $client")
    }

  }


  def doMatch(clients: List[Array[String]], orders: List[Array[String]]) = {

    val clientList: List[Client] = clients.map { array =>
      Client(
        name = array(0),
        Money = array(1).toInt,
        A = array(2).toInt,
        B = array(3).toInt,
        C = array(4).toInt,
        D = array(5).toInt
      )
    }
    var clientMap: scala.collection.mutable.Map[String, Client] = scala.collection.mutable.Map(
      clientList
        .groupBy(_.name)
        .mapValues(_.head)
        .toList: _*
    )
    val orderList: List[Order] = orders.zipWithIndex.map { case (array, i) =>
      Order(
        name = array(0),
        orderType = array(1),
        stockType = array(2),
        price = array(3).toInt,
        quantity = array(4).toInt,
        rn = i + 1
      )
    }
    matchOrders(orderList, clientMap)
    convertClients(clientMap)
  }
}
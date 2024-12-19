import org.scalatest.flatspec.AnyFlatSpec
import org.scalatest.matchers.should.Matchers

class StockSpec extends AnyFlatSpec with Matchers {

  "Stock" should "correctly match" in {

    val orders: List[Array[String]] = List(
      Array("C1", "s", "A", "100", "10"),
      Array("C2", "b", "A", "100", "10")
    )
    val clients: List[Array[String]] = List(
      Array("C1", "0", "10", "0", "0", "0"),
      Array("C2", "1000", "0", "0", "0", "0")
    )

    Stock.doMatch(clients, orders) should equal(
      List(
        List("C1", "1000", "0", "0", "0", "0"),
        List("C2", "0", "10", "0", "0", "0")
      )
    )

  }

}
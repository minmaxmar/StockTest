import scala.io.Source
// 0
// 1
// 2
object Main {
  private def readTxt(path: String): List[Array[String]] = {
    val source = Source.fromFile(path)
    val rows: List[Array[String]] = source.getLines().toList.map(line => line.split("\t"))
    source.close()
    rows
  }
  def main(args: Array[String]): Unit = {
    val clients = readTxt("C:\\Users\\79170\\Downloads\\Telegram Desktop\\test_scala-1_2\\clients.txt")
    val orders = readTxt("C:\\Users\\79170\\Downloads\\Telegram Desktop\\test_scala-1_2\\orders.txt")
    val pth = "C:\\Users\\79170\\Downloads\\Telegram Desktop\\test_scala-1_2\\clientsUpd.txt"
    val ret = Stock.doMatch(clients, orders)
    Stock.writeResult(pth, ret)
  }
}
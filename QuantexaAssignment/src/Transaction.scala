import scala.io.Source
//Define a case class Transaction which represents a transaction
case class Transaction(
  transactionId:     String,
  accountId:         String,
  transactionDay:    Int,
  category:          String,
  transactionAmount: Double)

object Transaction {
  //The full path to the file to import
  val fileName = "C:/Users/Viktor/Desktop/transactions.txt"
  //The lines of the CSV file (dropping the first to remove the header)
  val transactionslines = Source.fromFile(fileName).getLines().drop(1)
  //Here we split each line up by commas and construct Transactions
  val transactions: List[Transaction] = transactionslines.map { line =>
    val split = line.split(',')
    Transaction(split(0), split(1), split(2).toInt, split(3), split(4).toDouble)
  }.toList

  def totalPerDay {
    var curr = 1
    var total = 0.00
    for (a <- transactions) {
      if (a.equals(transactions.last)) println("Day: " + curr + " Total: " + (total+a.transactionAmount))
      else if (a.transactionDay == curr) total += a.transactionAmount
      else {
        println("Day: " + curr + " Total: " + total)
        curr += 1
        total = a.transactionAmount
      }
    }
  }

}

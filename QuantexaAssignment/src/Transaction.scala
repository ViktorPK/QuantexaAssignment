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
    val transactionsPerDay = transactions.groupBy(tr => tr.transactionDay)
    val totalsPerDay = transactionsPerDay.mapValues(list=>list.map(x=>x.transactionAmount).sum)
    val result = totalsPerDay.toSeq.sortBy(_._1)
    for (x<-result){
      println("Day: " + x._1 + " Total: " + x._2)
    }
  }
  def totalPerAccount {
    val transactionsByAccount = transactions.groupBy(tr => tr.accountId)
    val valuesByAccount = transactionsByAccount.mapValues(list => list.map(tr => (tr.category, tr.transactionAmount)))
    val transactionsPerType = valuesByAccount.mapValues(list => list.groupBy(y => y._1))
    val averagesPerType = transactionsPerType.mapValues(x => x.mapValues(list => (list.map { case ((category, value)) => value }.sum / list.length)))
    for (tr <- averagesPerType.toSeq.sortBy(_._1.split("A").last.toInt)) {
    val sorted = tr._2.toSeq.sortBy(_._1).mkString(" ").filterNot(x=>(x=='(' || x==')')).replace(",","->")
      println("AccountId: " + tr._1 + " AveragesPerType: " + sorted)
    }
    //println(averagesPerType.head)
    //TODO: check return types and add comments
    //TODO: print document
  }

  def statistics {
    val transactionsByAccount = transactions.groupBy(tr => tr.accountId)
    val grouped = transactions.groupBy(tr => (tr.transactionDay, tr.accountId)).toSeq.sortBy(tr => (tr._1._1, tr._1._2.split("A").last.toInt))
    val valuesByAccount = transactionsByAccount.mapValues(list => list.map(tr => (tr.transactionDay, tr.category, tr.transactionAmount)))
    val transactionsPerDay = valuesByAccount.mapValues(list => list.groupBy(y => y._1))

    for (tr <- grouped) {
      val thisAcc = transactionsPerDay.get(tr._1._2).head
      val last5days = thisAcc.filterKeys(x => (x < tr._1._1) && (x >= tr._1._1 - 5)).values.toList.flatten // list of transactions of this Account over last 5 days
      val errorMessage = "No Data Available"
      if (last5days.length > 0) {
        val max = last5days.maxBy(_._3)._3
        val average = last5days.map(_._3).sum / last5days.length
        val aas = last5days.filter(x => x._2 == "AA").map(_._3).sum
        val ccs = last5days.filter(x => x._2 == "CC").map(_._3).sum
        val ffs = last5days.filter(x => x._2 == "FF").map(_._3).sum
        println(tr._1._1 + " " + tr._1._2 + " " + max + " " + average + " " + aas + " " + ccs + " " + ffs)
      } else println(tr._1._1 + " " + tr._1._2 + " " + errorMessage);
    }

  }
  //TODO add comments
  //TODO print document

}

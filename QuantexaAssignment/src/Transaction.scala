import scala.io.Source
import java.io.File
import java.io.PrintWriter
//Define a case class Transaction which represents a transaction
case class Transaction(
  transactionId:     String,
  accountId:         String,
  transactionDay:    Int,
  category:          String,
  transactionAmount: Double)

 //Define a companion object for case class Transaction which holds the values used and the methods for the questions
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

  //method for question one - prints the total transaction values for all transactions each day to console and to a txt file
  def totalPerDay {
    val transactionsPerDay = transactions.groupBy(tr => tr.transactionDay)
    val totalsPerDay = transactionsPerDay.mapValues(list => list.map(x => x.transactionAmount).sum)
    val result = totalsPerDay.toSeq.sortBy(_._1)

    //Print result to console
    for (x <- result) {
      println("Day: " + x._1 + " Total: " + x._2)
    }

    //Print result to a csv file named totalPerDay.txt (file created in project folder)
    val writer = new PrintWriter(new File("totalPerDay.txt"))
    writer.write(result.mkString("Day, Value \n", "\n", "").filterNot(x => (x == '(' || x == ')')))
    writer.close()
  }

  //method for question two - prints the average value of transaction per account for each type of transaction to console and to a file
  def totalPerAccount {
    val transactionsByAccount = transactions.groupBy(tr => tr.accountId)
    val valuesByAccount = transactionsByAccount.mapValues(list => list.map(tr => (tr.category, tr.transactionAmount)))
    val transactionsPerType = valuesByAccount.mapValues(list => list.groupBy(y => y._1))
    val averagesPerType = transactionsPerType.mapValues(x => x.mapValues(list => (list.map { case ((category, value)) => value }.sum / list.length)))
    val sortedAveragesPerType = averagesPerType.toSeq.sortBy(_._1.split("A").last.toInt)
    
    //Print result in console
    for (tr <- sortedAveragesPerType) {
      val sortedCategories = tr._2.toSeq.sortBy(_._1).mkString(" ").filterNot(x => (x == '(' || x == ')')).replace(",", "->")
      println("AccountId: " + tr._1 + " AveragesPerType: " + sortedCategories)
    }

    //Print result to a csv file named totalPerAccount.txt
    val writer = new PrintWriter(new File("totalPerAccount.txt"))
    val sorted = averagesPerType.toSeq.sortBy(_._1.split("A").last.toInt).map(x => x._1 + "," + x._2.toSeq.sortBy(_._1).mkString("", ",", "\n").filterNot(x => (x == '(' || x == ')'))).mkString("AccountId, AveragesPerType \n", "", "")
    writer.write(sorted)
    writer.close()
  }
//TODO: optimise output to provide averages for each category
  //method for question three - prints the required statistics to console and to a file
  def statistics {
    val transactionsByAccount = transactions.groupBy(tr => tr.accountId)
    val grouped = transactions.groupBy(tr => (tr.transactionDay, tr.accountId)).toSeq.sortBy(tr => (tr._1._1, tr._1._2.split("A").last.toInt))
    val valuesByAccount = transactionsByAccount.mapValues(list => list.map(tr => (tr.transactionDay, tr.category, tr.transactionAmount)))
    val transactionsPerDay = valuesByAccount.mapValues(list => list.groupBy(y => y._1))
    val writer = new PrintWriter(new File("statistics.txt"))
    writer.write("Day, AccountId, Maximum, Average, AA Total Value, CC Total Value, FF Total Value \n")
    
    for (tr <- grouped) {
      val thisAcc = transactionsPerDay.get(tr._1._2).head
      val last5days = thisAcc.filterKeys(x => (x < tr._1._1) && (x >= tr._1._1 - 5)).values.toList.flatten // list of transactions of this Account over last 5 days
      val errorMessage = "NoDataAvailable"
      if (last5days.length > 0) {
        val max = last5days.maxBy(_._3)._3
        val average = last5days.map(_._3).sum / last5days.length
        val aas = last5days.filter(x => x._2 == "AA").map(_._3).sum
        val ccs = last5days.filter(x => x._2 == "CC").map(_._3).sum
        val ffs = last5days.filter(x => x._2 == "FF").map(_._3).sum
        val line = tr._1._1 + " " + tr._1._2 + " " + max + " " + average + " " + aas + " " + ccs + " " + ffs
        
        //Print to console
        println(line)
        
        //Print to document
        writer.write(line.replace(" ", ",") + "\n")
      } else {
        val line = tr._1._1 + " " + tr._1._2 + " " + errorMessage
        println(line)
        writer.write(line.replace(" ", ",") + "\n")
      }
    }
    writer.close()
  }
}

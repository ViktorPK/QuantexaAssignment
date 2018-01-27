
object Main {
  def main(args: Array[String]): Unit = {
    var br=0;
    for (a <- Transaction.transactions){
      if (a.transactionDay<=30) br+=1
    }
    println(br)
  }
}
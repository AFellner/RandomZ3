import scala.util.Random

object RandomDIMACStoZ3 {

  
  def main(args: Array[String]) 
  {
    writeFormulaToFile(80,5,8,"testDIMACS")
    println(Executer.executeZ3("testDimacs", "/dimacs").print)
  }
  
  def writeFormulaToFile(clauses: Int, variableRange: Int,maxLength: Int,fileName: String) =
  {
    Writer.writeToFile(fileName,generateFormula(clauses,variableRange,maxLength))
  }
  
  def generateFormula(clauses: Int, variableRange: Int,maxLength: Int):String =
  {
    var formula: String = ""
    formula += "p cnf "+ variableRange + " " + clauses + "\n"
    for (i <- 0 to clauses-1){
      formula += generateClause(variableRange,maxLength) + "0\n"
    }
    formula
  }
   
   def generateClause(variableRange: Int, maxLength: Int):String =
   {
     var r = new Random()
     var clause = ""
     var length = r.nextInt(maxLength)
     if (length == 0) length = 1
     for (i <- 0 to length-1)
     {
       if (r.nextBoolean)
    	   clause += r.nextInt(variableRange-1)+1 + " "
       else
           clause += "-" + r.nextInt(variableRange-1)+1 + " "
     }
     clause
   }
}
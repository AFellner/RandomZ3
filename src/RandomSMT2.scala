import scala.sys.process._
import scala.util.Random

object RandomFormula {

  def main(args: Array[String]) 
  {
    writeFormulaToFile(14,6,"test")
    try
    {
    	printProof(Executer.executeZ3("test","/smt2"))
    }
    catch
    {
      case e: Exception => println("formula is satisfiable -> no proof generated");
    }
  }
  
  def printProof(stream: Stream[String]) =
  {
    
    for (i <- 2 to stream.length-1)
    {
      println(stream(i))
    }
  }
 
  
  def writeFormulaToFile(conDepth: Int, variableRange: Int,fileName: String) =
  {
    Writer.writeToFile(fileName,generateFormula(conDepth,variableRange))
  }
  
  def generateFormula(conDepth: Int, variableRange: Int):String =
  {
    var formula: String = ""
    formula += "(set-logic QF_UF)\n"
    formula += "(set-option :produce-unsat-cores true)\n"
    formula += "(set-option :produce-proofs true)"
    for (i <- 0 to variableRange){
      formula += "(declare-const p_" + i + " Bool)\n"
    }
    val tmp = connectives(conDepth,variableRange)
    formula += "(assert "+ tmp + ")\n"
    //formula += "(assert (not "+tmp+"))"
    formula += "(check-sat)\n"
    formula += "(get-proof)\n"
    formula += "(get-unsat-core)"
    formula
  }
  
  def connectives(conDepth: Int, variableRange: Int): String =
  {
    var out: String = ""
    var r = new Random()
    val usecon = r.nextInt(10)
    if (usecon > 1 && conDepth > 0)
    {
      val con:Int = r.nextInt(7)
      val tmpCon =  con match {
        case 0 => " true "
        case 1 => " false "
        case 2 => "(not " + connectives(conDepth-1,variableRange) + ")"
        case 3 => "(and " + connectives(conDepth-1,variableRange) + " " + connectives(conDepth-1,variableRange) + ")"
        case 4 => "(or " + connectives(conDepth-1,variableRange) + " " + connectives(conDepth-1,variableRange) + ")"
        case 5 => "(=> " + connectives(conDepth-1,variableRange) + " " + connectives(conDepth-1,variableRange) + ")"
        case 6 => "(ite " + connectives(conDepth-1,variableRange) + " " + connectives(conDepth-1,variableRange) + " " + connectives(conDepth-1,variableRange)+  ")"
        case _ => "(and " + connectives(conDepth-1,variableRange) + " " + connectives(conDepth-1,variableRange) + ")"
      }
      out += tmpCon
    }
    else
    {
      out += "p_"+r.nextInt(variableRange)
    }
    out
  }
}
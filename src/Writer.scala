import java.io._

object Writer {

 
  def using[A <: {def close(): Unit}, B](param: A)(f: A => B): B =
  try { f(param) } finally { param.close() }
  
  def writeToFile(fileName:String, data:String) = 
  using (new FileWriter(fileName)) {
    fileWriter => fileWriter.write(data)
  }

  def appendToFile(fileName:String, textData:String) =
  using (new FileWriter(fileName, true)){ 
    fileWriter => using (new PrintWriter(fileWriter)) {
      printWriter => printWriter.println(textData)
    }
  }
}
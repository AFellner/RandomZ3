import scala.sys.process._
object Executer {

  def executeZ3(fileName: String,parameters: String): Stream[String] =
  {
    val call = "z3 " + parameters + " " + fileName
    call.!!
    val contents = Process(call).lines
    contents
  }
}
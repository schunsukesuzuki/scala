import scala.io.StdIn.readLine
object Main extends App{
  override def main(args: Array[String]): Unit = {
      marking
  }
  /* print (" plz enter number ")  */
  var i = readInt()
  def marking = {
    if(0 < i && i < 100) {
      for ( j <- 1 to i ) {
      print( "*" )
      }
    }
  }
}
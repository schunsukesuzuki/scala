import scala.io.StdIn.readLine
object Main extends App{
  override def main(args: Array[String]): Unit = {
    marking
  }
  def marking = {
  /* print (" plz enter number ") */
  var i = readInt()    
    if(1 < i && i < 101) { 
      for ( j <- 1 to i ) {
      print( "*" )
      }
    }
    println("")
  }
}
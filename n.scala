import scala.io.StdIn.readLine

object n {
  def main(args: Array[String]): Unit = {
    marking
  }
  /* print (" plz enter number ")  */
  var i = readInt()
  
  def marking = {
    if(0 < i && i < 100) { 
      for ( j <- 1 to i ) {
      print( " * " )
      }
    }
  }
}
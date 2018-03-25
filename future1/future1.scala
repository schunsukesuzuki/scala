import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Main extends App {
  val msg = "hello"
  val f: Future[String] = Future {
    Thread.sleep(1000)
    msg * 5
  }

  println(f.isCompleted) // false
  val result1: Option[Try[String]] = f.value
  println(result1) 

  Thread.sleep(2000)

  println(f.isCompleted) // true
  val result2: Option[Try[String]] = f.value
  println(result2.get.get) 
}
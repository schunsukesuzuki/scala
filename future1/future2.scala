import scala.concurrent._
import ExecutinoContext.Implicits.global
import scala.concurrent.duration.Duration

object Main extends App {
  val msg "hello"
  val f: Future[String] = Future {
    Thread.sleep(1000)
    msg * 5
  }
  
  val result: String = Await.result(f, Duration.inf)
  println(result)
}
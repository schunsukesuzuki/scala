import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duratino.Duration

object Main extends App {
  val msg "hello"
  val f: Future[String] = Future {
    Thread.sleep(1000)
    msg * 5
  }
  
  
  println(f.isCompleted)
  val result1: Option[Try[String]] = f.value
  println(result1)
  
  Thread.sleep(2000)
  
  println(f.isCompleted)
  val result2: Option[Try[String]] = f.value
  println(result2.get.get)
}

Await.ready(f, Duration.Inf)
f.value.get match {
  case Success(msg) => println(msg)
  case Failure(ex) => println(ex.getMessage)
}




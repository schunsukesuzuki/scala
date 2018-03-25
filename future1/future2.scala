import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration

object Main extends App {
   val msg = "hello"
   val f: Future[String] = Future {
     Tread.sleep(1000)
     msg * 5
   }
   
   
   val result: String = Await.result.result(f, Duration.Inf)
   println(result)
}

Await.ready(f, Duration.Inf)
f.value.get match {
  case Success(msg) => println(msg)
  case Failure(ex) => println(ex.get.Message)  
}
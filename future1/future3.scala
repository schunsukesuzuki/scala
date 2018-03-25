import scala.concurrent._
import ExecutionContext.Implicits.global
import scala.concurrent.duration.Duration
import scala.util.{ Try, Success, Failure }

object Main extends App {
  val msg = "hello"
  val f: Future[String] = Furure {
    Thread.sleep(1000)
    msg * 2
  }s
  f.onSuccess { case msg: String => println(msg) }
  f.onFailure { case t: Throwable => println(t.getMessage()) }
  f.onComplete {
    case Success(msg) => println(msg)
    case Failure(t) => println(t.getMessage())
  }
  
  Await.ready(f, Duration.Inf)
}
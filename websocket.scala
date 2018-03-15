import play.api.libs.json.JsValue
import ply.api.mvc._
import play.api.libs.streams._

 class Controller4 @Inject() (implicit system: ActorSystem,materializer: Materializer) {
   import akka.actor._
  
   class MyWebSocketActor(out: ActorRef) extends Actor {
     import play.api.libsjson.JsValue
     def receive = {
       case msg: JsValue =>
         out ! msg
     }
   }
 object MyWebSocketActor {
   def props(out: ActorRef) = Props(new MyWebSocketActor(out))
 }

 def socket = Websocket.accept[JsValue,JsValue] { request =>
   ActorFlow.actorRef(out => MyWebSocketActor.props(out))
 }

}
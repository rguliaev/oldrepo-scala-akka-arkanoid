package controllers

import java.security.MessageDigest
import java.util.Date
import javax.inject._

import actors.MyWebSocketActor
import akka.actor.ActorSystem
import akka.stream.Materializer
import akka.stream.scaladsl.Flow
import play.api._
import play.api.libs.json.JsValue
import play.api.libs.streams.ActorFlow
import play.api.mvc._

import scala.concurrent.{ExecutionContext, Future}

@Singleton
class HomeController @Inject()(implicit system: ActorSystem, materializer: Materializer) extends Controller with ApiSecurity {
  def index = Action.async { implicit request =>
    Future.successful(
      Ok(views.html.index())
    )
  }

  def websocket = WebSocketRequest
}

trait ApiSecurity { this: Controller =>
  private[this] val tokenName = "sessionId"
  private[this] def hash(s: String) = MessageDigest.getInstance("SHA-256").digest(s.getBytes("UTF-8")).map("%02x".format(_)).mkString("")
  implicit class ResultWithToken(result: Result)(implicit request: RequestHeader) {
    def withToken: Result = {
      request.session.get(tokenName) match {
        case Some(sessionId) => result
        case None => result.withSession(tokenName -> hash(new Date().getTime + request.remoteAddress))
      }
    }
  }

  def WebSocketRequest(implicit system: ActorSystem, materializer: Materializer) =
    WebSocket.acceptOrResult[JsValue, JsValue] { request =>
      Future.successful(request.session.get(tokenName) match {
        case None => Left(Forbidden)
        case Some(_) => Right(ActorFlow.actorRef(MyWebSocketActor.props))
      })
  }
}

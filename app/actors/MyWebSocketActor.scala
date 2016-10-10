package actors

import akka.actor.{Actor, Props, _}
import java.awt.Rectangle
import scala.language.postfixOps
import play.api.libs.json._
import scala.util.Random

object MyWebSocketActor {
  def props(out: ActorRef) = Props(new MyWebSocketActor(out))
}

class MyWebSocketActor(out: ActorRef) extends Actor {
  println("WebSocketActor has been started " + self.path)
  val gameActor = context.actorOf(Props(new Game), "gameActor")
  def receive = {
    case msg: JsValue =>
      (msg \ "command").as[String] match {
        case "start" =>
          gameActor ! Start
        case "stop" =>
          gameActor ! Stop
      }
    case svgData: SvgData =>
      out ! JsObject(Map("svg" -> JsString(svgData.svg)))
  }
}

sealed trait GameStuff
sealed trait PanelSize extends GameStuff {
  val panelWidth = 600
  val panelHeight = 400
}
case object Start extends GameStuff
case object Stop extends GameStuff
case class Direction(x: Int, y: Int) extends GameStuff
case class SvgData(svg: String) extends GameStuff
case class GameData(bricks: List[Brick], ball: Ball, direction: Direction) extends GameStuff with PanelSize {
  def generate =
    bricks.find(b => b.visible && b.getRect.intersects(ball.getRect)).map { b =>
      this.copy(bricks = bricks.filter(_ != b),direction = direction.copy(y = - direction.y))
    } getOrElse {
      if ((ball.ballX - ball.width) <= 0 || ball.ballX + ball.width >= panelWidth)
        this.copy(direction = direction.copy(x = - direction.x))
      else if ((ball.ballY - ball.height) <= 0 || ball.ballY + ball.height >= panelHeight)
        this.copy(direction = direction.copy(y = - direction.y))
      else this
    }

  def go = this.copy(ball = ball.copy(ballX = ball.ballX + direction.x, ballY = ball.ballY + direction.y)).generate
}

class Game extends Actor with PanelSize {
  def ball = Ball(Random.nextInt(500) + 50, (250 to 350)(Random.nextInt(100)))
  val dirList = List(-1, 1)
  def dirCoord = dirList(Random.nextInt(dirList.size))
  def direction = Direction(dirCoord, dirCoord)
  def bricks: List[Brick] = {
    val rows = 3
    val cols = 5
    val gapBetweenBricks = 10
    val totalGap = (cols + 1) * 10
    val spaceForBrick = panelWidth - totalGap
    val widthOfBrick = spaceForBrick / cols
    val heightOfBrick = 10
    val currentX = 10
    val currentY = 70

    def createRow(n: Int, y: Int): List[Brick] = {
      (1 to n).foldLeft(List[Brick]()) { (res, n) =>
        val x = currentX + (widthOfBrick + gapBetweenBricks) * (n - 1)
        res :+ new Brick(x, y, widthOfBrick, heightOfBrick)
      }
    }

    (1 to rows).foldLeft(List[Brick]()) { (res, n) =>
      res ++ createRow(cols, currentY + (gapBetweenBricks * 2) * (n - 1))
    }
  }

  def draw(gameData: GameData): String = {
    s"""
    <svg id="svg"  xmlns="http://www.w3.org/2000/svg" version="1.1" height = "${panelHeight}px"  width = "${panelWidth}px">
    <rect x="0" y="0" width="${panelWidth}" height="${panelHeight}" style="fill:#eee;stroke:black;stroke-width:2;" />
    ${gameData.bricks.filter(_.visible).foldLeft(""){(res, n) => res + n.draw }}
    ${gameData.ball.draw}
    </svg>"""
  }

  def receive = stopped

  def started: Receive = {
    case gd: GameData =>
      Thread.sleep(5) //for proper speed
      if (!gd.bricks.forall(!_.visible)) self ! gd.go
      context.parent ! SvgData(draw(gd))
    case Stop => context.become(stopped)
  }

  def stopped: Receive = {
    case Start =>
      context.become(started)
      self ! GameData(bricks,ball,direction).go
  }
}

case class Ball(ballX: Int, ballY: Int) {
  val width = 10
  val height = 10
  def draw = s"""<circle cx="$ballX" cy="$ballY" r="10" stroke="black" stroke-width="1" fill="#81D67E" />"""
  def getRect = new Rectangle(ballX, ballY, width, height)
}

case class Brick(x: Int, y: Int, width: Int, height: Int, visible: Boolean = true) {
  def draw = s"""<rect x="$x" y="${y - height}" width="$width" height="$height" style="fill:#fff;stroke:black;stroke-width:1;" />"""
  def getRect = new Rectangle(x, y, width, height)
}



package example

import org.scalajs.dom
import org.scalajs.dom.html
import scala.util.Random
import scala.scalajs.js
import js.annotation.JSExport

case class Point(x: Double, y: Double){
  def +(p: Point) = Point(x + p.x, y + p.y)
  def -(p: Point) = Point(x - p.x, y - p.y)
  def *(d: Double) = Point(x * d, y * d)
  def /(d: Double) = Point(x / d, y / d)
  def length = Math.sqrt(x * x + y * y)
  def normalize = if (length != 0 ) this./(length)
    else this
  def distance(p: Point) = this.-(p).length
  def invert = Point(-x, -y)
}

@JSExport
object ScalaJSExample {

  case class Agent(pos: Point, v: Point)

  case class Swarm(agents: List[Agent]){
    val repulsionRadius = 10  // within this radius, agent is repelled from others
    val alignmentRadius = 25  // within this radius, agent wants to align his heading
    val neighborRadius = 40   // within this radius, agent seeks company of others
    val maxSpeed = 3
  }


  var startTime = js.Date.now()

  val canvas = dom.document
                  .getElementById("canvas")
                  .asInstanceOf[html.Canvas]
  val ctx = canvas.getContext("2d")
                  .asInstanceOf[dom.CanvasRenderingContext2D]

  var enemies = Seq.empty[Agent]


  def calc(swarm: Swarm): Swarm = {

    canvas.height = dom.window.innerHeight.toInt
    canvas.width = dom.window.innerWidth.toInt

    def move(agent: Agent, others: List[Agent]): Agent = {

      val tooClose: List[Point] = others
        .filter(other => agent.pos.distance(other.pos) < swarm.repulsionRadius)
        .map(_.pos)

      val sumVectors = tooClose.foldLeft(Point(0,0)){ (a,b) =>
        a + (b - agent.pos)
      }
      val normalized = sumVectors.normalize
      val inverseVector = normalized.invert * swarm.maxSpeed
      val tooCloseImpulse = agent.v + inverseVector

      println(s"agent $agent")
      println(s"tooClose $tooClose")
      println(s"sumVectors $sumVectors")
      println(s"normalized $normalized")
      println(s"inverseVector $inverseVector")
      println(s"tooCloseImpulse $tooCloseImpulse")
      println("-")

      val neighbors: List[Point] = others
        .filter(other => agent.pos.distance(other.pos) < swarm.neighborRadius)
        .map(_.pos)

      val aggregateImpulse =
        if (neighbors.isEmpty)
          Point(0,0)
        else {
          val (avgX, avgY) = (neighbors.map(_.x).sum / neighbors.size, neighbors.map(_.y).sum / neighbors.size )
          val centerOfNeighbors = Point(avgX, avgY)
          val vectorTowardsNeighborsNormalized = (centerOfNeighbors - agent.pos).normalize
          val vectorTowardsNeighbors = vectorTowardsNeighborsNormalized * swarm.maxSpeed / 2
          vectorTowardsNeighbors
        }

      val newSpeed = agent.v + tooCloseImpulse + aggregateImpulse
      val newSpeedMax = newSpeed.normalize * swarm.maxSpeed

      agent.copy(pos = agent.pos + newSpeedMax,
        v = newSpeedMax)
    }

    val processedAgents = swarm.agents.map{a =>
      move(a,swarm.agents.filterNot(_ == a))
    }

    Swarm(processedAgents)
  }

  def deltaT = ((js.Date.now() - startTime) / 1000).toInt

  def draw(swarm: Swarm) = {
    ctx.fillStyle = "white"

    ctx.fillRect(0, 0, canvas.width, canvas.height)

    ctx.fillStyle = "blue"
//    for (agent <- swarm.agents){
//      ctx.fillRect(agent.pos.x - 10, agent.pos.y - 10, 20, 20)
//    }

    for (agent <- swarm.agents){
      ctx.beginPath();
//      ctx.strokeStyle = 'blue'
      ctx.moveTo(agent.pos.x,agent.pos.y);
      ctx.lineTo(agent.pos.x + agent.v.x * 3,agent.pos.y + agent.v.y * 3);
      ctx.stroke();

      //ctx.fillStyle = "white"
      //ctx.fillText(agent.v.x + " " + agent.v.y, agent.pos.x + 10, agent.pos.y - 10)
    }


    ctx.fillStyle = "white"
  }

  @JSExport
  def main(): Unit = {
    dom.console.log("main")

    canvas.height = dom.window.innerHeight.toInt
    canvas.width = dom.window.innerWidth.toInt

//    dom.document.onmousemove = { (e: dom.MouseEvent) =>
//      player = Point(e.clientX.toInt, e.clientY.toInt)
//      (): js.Any
//    }

    val agents = (1 to 80).map{i=>
      Agent(pos = Point(Random.nextInt(canvas.width), Random.nextInt(canvas.height)),
        v = Point( -3 + Random.nextDouble * 6, -3 + Random.nextDouble * 6))
    }.toList

    var swarm = Swarm(agents)

    dom.window.setInterval(() => {
      swarm = calc(swarm)
      draw(swarm)}, 20)
  }
}



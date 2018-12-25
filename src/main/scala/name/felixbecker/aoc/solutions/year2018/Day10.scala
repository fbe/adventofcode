package name.felixbecker.aoc.solutions.year2018

import scala.annotation.tailrec
import scala.io.Source

case class SkyPoint(positionX: Int, positionY: Int, velocityX: Int, velocityY: Int)

object Day10 extends App {
  val pattern = """position=<\s*([0-9-]+)\s*,\s*([0-9-]+)\s*> velocity=<\s*([0-9-]+),\s*([0-9-]+)\s*>""".r
  val skyPoints = Source.fromResource("2018/day10/input.txt").getLines().map {
    case pattern(px, py, vx, vy) => SkyPoint(px.toInt,py.toInt,vx.toInt,vy.toInt)
  }.toList


  def nextSecond(skyPoints: List[SkyPoint]): List[SkyPoint] = {
    skyPoints.map { point =>
      SkyPoint(point.positionX + point.velocityX, point.positionY + point.velocityY, point.velocityX, point.velocityY)
    }
  }

  def minMaxXY(skyPoints: List[SkyPoint]): MinXY = {
    val minX = skyPoints.minBy(_.positionX).positionX
    val minY = skyPoints.minBy(_.positionY).positionY

    val maxX = skyPoints.maxBy(_.positionX).positionX
    val maxY = skyPoints.maxBy(_.positionY).positionY

    MinXY(minX, minY, maxX, maxY)
  }

  case class MinXY(minX: Int, minY: Int, maxX: Int, maxY: Int){
    def <= (other: MinXY): Boolean = {
      minX >= other.minX && minY >= other.minY && maxX <= other.maxX && maxY <= other.maxY
    }
  }

  @tailrec
  def calc(secondsPassed: Int, lastMinXY: MinXY, lastSkyPoints: List[SkyPoint]): Unit = {
    val nextIteration = nextSecond(lastSkyPoints)
    val nextMinXy = minMaxXY(nextIteration)
    if(nextMinXy <= lastMinXY){
      calc(secondsPassed + 1, nextMinXy, nextIteration)
    } else {
      println(s"Seconds passed: $secondsPassed")
      (lastMinXY.minY to lastMinXY.maxY).foreach { currY =>
        (lastMinXY.minX to lastMinXY.maxX).foreach { currX =>
          lastSkyPoints.find(p => p.positionX == currX && p.positionY == currY) match {
            case Some(_) => print("#")
            case None => print(".")
          }
        }
        println()
      }
    }
  }

  calc(0, minMaxXY(skyPoints), skyPoints)


}

package name.felixbecker.aoc.solutions.year2018
import scala.io.Source

case class Coordinate(identifier: String, location: Point)

case class Point(x: Int, y: Int, nearestCoordinate: Option[Coordinate] = None){
  def manhattanDistanceTo(p: Point): Int = {
    Math.abs(x - p.x) + Math.abs(y - p.y)
  }
  def touchesFieldCorner(maxPoint: Point, minPoint: Point): Boolean = {
    x == maxPoint.x || x == minPoint.x || maxPoint.y == y || minPoint.y == y
  }

  def distanceToAllPointsIsLessThan(num: Int, coordinates: List[Coordinate]): Boolean = {
    coordinates.map { c => c.location.manhattanDistanceTo(this) }.sum < num
  }
}

class GameField(val coordinates: List[Coordinate]) {
  val minPoint = Point(coordinates.minBy(_.location.x).location.x, coordinates.minBy(_.location.y).location.y)
  val maxPoint = Point(coordinates.maxBy(_.location.x).location.x, coordinates.maxBy(_.location.y).location.y)
  val allPoints: Set[Point] = (minPoint.x to maxPoint.x).flatMap { x =>
    (minPoint.y to maxPoint.y).map { y =>
      val currentPoint = Point(x,y)
      val distanceWithCoordinate = coordinates.map { c => c -> c.location.manhattanDistanceTo(currentPoint)}.groupBy(dc => dc._2)
      val closestCoordinate = distanceWithCoordinate.minBy(_._1)._2 match {
        case one :: Nil => Some(one._1)
        case _ :: _ => None
      }
      Point(x,y, closestCoordinate)
    }
  }.toSet

  val coordinatesWithoutCornerTouchingField: List[Coordinate] = coordinates.filterNot { c =>
    allPoints.count { p => p.touchesFieldCorner(minPoint, maxPoint) && p.nearestCoordinate.contains(c)} > 0
  }

  val largestAreaSize: Int = coordinatesWithoutCornerTouchingField.map { c =>
    allPoints.count(_.nearestCoordinate.contains(c))
  }.max

  val solution2Count: Int = allPoints.count(p => p.distanceToAllPointsIsLessThan(10000, coordinates))

}

object Day6 extends App {

  val enoughNames = (1 to Integer.MAX_VALUE).iterator.map(_.toString)

  val coordinates = Source.fromResource("2018/day6/input.txt").getLines().map { coordPair =>
    val coord = coordPair.split(", ")
    Coordinate(enoughNames.next, Point(coord(0).toInt, coord(1).toInt))
  }.toList

  implicit val gameField: GameField = new GameField(coordinates)

  println(gameField.largestAreaSize)

  println(s"Solution 2: ${gameField.solution2Count}")
}
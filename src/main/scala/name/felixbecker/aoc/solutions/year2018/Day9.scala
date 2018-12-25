package name.felixbecker.aoc.solutions.year2018

import scala.io.Source

object Day9 extends App {

  val regex = """([0-9]+) players; last marble is worth ([0-9]+) points""".r

  val (playerNumber, points) = Source.fromResource("2018/day9/input.txt").getLines().mkString match {
    case  regex(pn, p) => (pn, p)
  }

  println(playerNumber)
  println(points)
}

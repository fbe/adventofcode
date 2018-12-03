package name.felixbecker.aoc.solutions.year2018

import scala.io.Source

object Day3 extends App {

  val minExecutionTime = (1 to 100).map { _ =>
    val start = System.currentTimeMillis()
    // First
    case class Field(x: Int, y: Int)
    case class Claim(id: Int, xoffs: Int, yoffs: Int, xsize: Int, ysize: Int) {
      def isDefinedAt(f: Field): Boolean = f.x > xoffs & f.x <= xoffs + xsize && f.y > yoffs && f.y <= yoffs + ysize

      def getCoveredFields: Set[Field] = (xoffs until xoffs + xsize).flatMap { x => (yoffs until yoffs + ysize).map { y => Field(x, y) } }.toSet
    }

    // the game doesn't start at 0, so we simply take 1000 fields (+1), and YES, that is waste of memory, but it makes
    // the code more readable
    val entireField = Array.fill(1009, 1000)(0)
    val pattern = """^#([0-9]+)\s+@\s+([0-9]+),([0-9]+):\s+([0-9]+)x([0-9]+)$""".r
    var moreThanOneFields = 0
    val claims = Source.fromResource("2018/day3/input.txt").getLines().map { case pattern(id, xoffs, yoffs, xsize, ysize) =>
      val c = Claim(id.toInt, xoffs.toInt, yoffs.toInt, xsize.toInt, ysize.toInt)

      (c.xoffs until c.xoffs + c.xsize).foreach { x =>
        (c.yoffs until c.yoffs + c.ysize).foreach { y =>
          entireField(x)(y) += 1
          if (entireField(x)(y) == 2) moreThanOneFields += 1
        }
      }

      c
    }.toList

    println(moreThanOneFields)

    // Second
    claims.find(c => c.getCoveredFields.count(f => entireField(f.x)(f.y) > 1) == 0).foreach { c =>
      println(s"Claim ID (which doesn't overlap): ${c.id}")
    }

    // Expected solutions: 97218 + 717
    val end = System.currentTimeMillis()
    println(s"Took ${end - start}ms")
    end-start
  }.min

  println(minExecutionTime)
}

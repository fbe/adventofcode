package name.felixbecker.aoc.solutions.year2018

import scala.io.Source

object Day3 extends App {

  // First
  case class Field(x: Int, y: Int)
  case class Claim(id: Int, xoffs: Int, yoffs: Int, xsize: Int, ysize: Int){
    def isDefinedAt(f: Field): Boolean = f.x > xoffs & f.x <= xoffs+xsize  && f.y > yoffs && f.y <= yoffs + ysize
    def getCoveredFields: Set[Field] = (xoffs+1 to xoffs+xsize).flatMap { x => (yoffs+1 to yoffs + ysize).map { y => Field(x,y)}}.toSet
  }

  val pattern = """^#([0-9]+)\s+@\s+([0-9]+),([0-9]+):\s+([0-9]+)x([0-9]+)$""".r
  val claims = Source.fromResource("2018/day3/input.txt").getLines().map { case pattern(id, xoffs, yoffs, xsize, ysize) =>
    Claim(id.toInt, xoffs.toInt, yoffs.toInt, xsize.toInt, ysize.toInt)
  }.toList

  val allFields = collection.mutable.Map((1 to 1000).flatMap { x => (1 to 1000).map { y => (Field(x,y), 0)}}.toMap.toSeq: _*)

  claims.foreach { c => c.getCoveredFields.foreach { f => allFields(f) = allFields(f) + 1}}

  println(allFields.count { case (f, x) => x > 1})

  // Second
  claims.find(c => c.getCoveredFields.count(f => allFields(f) > 1) == 0).foreach { c =>
    println(s"Claim ID (which doesn't overlap): ${c.id}")
  }

  // TODO further improvement: Use arrays instead of maps to speed up..
}

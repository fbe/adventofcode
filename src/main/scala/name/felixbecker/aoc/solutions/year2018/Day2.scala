package name.felixbecker.aoc.solutions.year2018

import scala.annotation.tailrec
import scala.io.Source

object Day2 extends App {

  val allBoxIds = Source.fromResource("2018/day2/input.txt").getLines.toList

  // Start 1

  var twoChars = 0
  var threeChars = 0

  allBoxIds
    .map(
      _.groupBy { v => v }.map { case (_, list) => list.length}.toSet
    ).foreach { groupedChars =>
      if(groupedChars.contains(2)) twoChars += 1
      if(groupedChars.contains(3)) threeChars += 1
    }

  println(s"Two chars: $twoChars")
  println(s"Three chars: $threeChars")
  println(s"Checksum ${twoChars*threeChars}")

  // Start 2

  @tailrec
  def diffsByOneCharAtTheSamePosition(firstString: String, secondString: String, searchIdx: Int = 0, diffs: Int = 0, lastMismatchIdx: Option[Int] = None): (Boolean, Option[Int]) = {
    assert(firstString.length == secondString.length)
    if(firstString == secondString){
      (false, None)
    } else if(searchIdx == firstString.length && diffs == 1){
      (true, lastMismatchIdx)
    } else if(searchIdx == firstString.length && diffs > 1){
      (false, None)
    } else {
      val fc = firstString(searchIdx)
      val sc = secondString(searchIdx)
      val newDiffs = if(fc != sc) diffs+1 else diffs
      val newLastSeenMismatch = if(fc != sc) Some(searchIdx) else lastMismatchIdx
      diffsByOneCharAtTheSamePosition(firstString, secondString, searchIdx+1, newDiffs, newLastSeenMismatch)
    }
  }

  val (z1, z2, z3) = allBoxIds.flatMap { l =>
    val f = allBoxIds.find(other => diffsByOneCharAtTheSamePosition(other, l)._1)
    f.map { x => (x, l, diffsByOneCharAtTheSamePosition(x,l)._2)}
  }.head

  println(s"matching chars without wrong char: ${z1.substring(0, z3.get)}${z1.substring(z3.get+1)}")





}

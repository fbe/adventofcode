package name.felixbecker.aoc.solutions.year2018

import scala.annotation.tailrec
import scala.io.Source

object Day1 extends App {

  // Start 1
  val frequencyChanges = Source.fromResource("2018/day1/input1.txt").getLines.map(_.toLong).toList
  val summedFrequencies = frequencyChanges.sum
  println(s"Part 1/2 (01.12.2018): $summedFrequencies")

  @tailrec
  def calculateFrequencyAndExitOnTwiceSeen(freqIt: Iterator[Long], freshIt: () => Iterator[Long], seenFreq: Set[Long] = Set(0), curr: Long = 0): Long = {
    val it = if(freqIt.hasNext) freqIt else freshIt()
    val newFreq = curr + it.next()
    if(seenFreq.contains(newFreq)) newFreq else calculateFrequencyAndExitOnTwiceSeen(it, freshIt, seenFreq + newFreq, newFreq)
  }

  val solution2 = calculateFrequencyAndExitOnTwiceSeen(frequencyChanges.iterator, () => frequencyChanges.iterator)

  /*
  +1, -1 first reaches 0 twice.
  +3, +3, +4, -2, -4 first reaches 10 twice.
  -6, +3, +8, +5, -6 first reaches 5 twice.
  +7, +7, -2, -7, -4 first reaches 14 twice.
   */

  val e1: List[Long] = List(+1, -1)
  val e2: List[Long] = List(+3, +3, +4, -2, -4)
  val e3: List[Long] = List(-6, +3, +8, +5, -6)
  val e4: List[Long] = List(+7, +7, -2, -7, -4)

  assert(calculateFrequencyAndExitOnTwiceSeen(e1.iterator, e1.iterator _) == 0)
  assert(calculateFrequencyAndExitOnTwiceSeen(e2.iterator, e2.iterator _) == 10)
  assert(calculateFrequencyAndExitOnTwiceSeen(e3.iterator, e3.iterator _) == 5)
  assert(calculateFrequencyAndExitOnTwiceSeen(e4.iterator, e4.iterator _) == 14)

  println(s"Part 2/2 (01.12.2018): $solution2")

}

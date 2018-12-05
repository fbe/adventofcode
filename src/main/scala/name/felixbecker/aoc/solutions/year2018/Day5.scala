package name.felixbecker.aoc.solutions.year2018

import java.util.concurrent.ConcurrentHashMap

import scala.io.Source

object Day5 extends App {

  val start = System.currentTimeMillis()
  val original = Source.fromResource("2018/day5/input.txt").getLines().mkString("")

  def reduce(input: String): String = {
    var strReduced: Boolean = true
    var inputStr = input

    while(strReduced){
      val strlenBeforeReplace = inputStr.length
      'a'.to('z').foreach { c =>
        inputStr = inputStr
          .replaceAll(s"${c.toLower}${c.toUpper}", "")
          .replaceAll(s"${c.toUpper}${c.toLower}", "")
      }
      strReduced = inputStr.length < strlenBeforeReplace
    }

    inputStr
  }

  // Solution 1 - 11546
  println(s"Solution 1: ${reduce(original).length}")

  // Solution 2
  val unitToCount = new ConcurrentHashMap[Char, Int]()

  'a'.to('z').par.foreach { c =>
    unitToCount.put(c, reduce(original
      .replaceAll(s"${c.toLower}", "")
      .replaceAll(s"${c.toUpper}", "")).length)
  }

  import scala.collection.JavaConverters._
  val minUnit = unitToCount.asScala.minBy(_._2)

  println(s"Solution 2: ${minUnit._2} - Removed maximum with unit ${minUnit._1}")

  println(s"Took ${System.currentTimeMillis() - start}ms")
}
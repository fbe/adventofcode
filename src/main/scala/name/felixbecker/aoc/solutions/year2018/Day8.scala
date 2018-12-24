package name.felixbecker.aoc.solutions.year2018

import scala.io.Source

case class Node(childNodes: List[Node], metaData: List[Int]) extends Traversable[Node] {
  override def foreach[U](f: Node => U): Unit = {
    f(this)
    childNodes.foreach(cn => cn.foreach(f))
  }

  override def toString(): String = s"Node (Metadata: $metaData), ChildNodes: ${childNodes.size}"
}

object Day8 extends App {

  //val input = Source.fromResource("2018/day8/input-sample.txt").getLines().toList
  val input = Source.fromResource("2018/day8/input.txt").getLines().toList

  assert(input.size == 1)

  val inputNumbers = input.head.split(" ").map(_.toInt).toList

  def parseTree(remainingInput: Iterator[Int]): Node = {
    val subNodesCount = remainingInput.next()
    val metaDataCount = remainingInput.next()
    val subNodes = (1 to subNodesCount).map(x => parseTree(remainingInput)).toList
    val metaData = (1 to metaDataCount).map(x => remainingInput.next).toList
    Node(subNodes, metaData)
  }

  val tree = parseTree(inputNumbers.iterator)

  val solution1 = tree.flatMap(_.metaData).sum

  println(solution1)

}
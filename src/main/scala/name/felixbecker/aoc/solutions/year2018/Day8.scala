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
    val metaData = remainingInput.take(metaDataCount).toList
    Node(subNodes, metaData)
  }

  val tree = parseTree(inputNumbers.iterator)

  val solution1 = tree.flatMap(_.metaData).sum

  println(s"Solution 1: $solution1")

  def calc(node: Node): Int = node match {
    case Node(Nil, metaData) => metaData.sum
    case Node(children, metaData) =>
      metaData.map {
        case x if x < 1 || x > children.size => 0
        case x => calc(children(x-1)) }.sum
  }

  println(s"Solution 2: ${calc(tree)}")

}
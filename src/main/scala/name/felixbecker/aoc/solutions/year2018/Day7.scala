package name.felixbecker.aoc.solutions.year2018

import name.felixbecker.aoc.solutions.year2018.Day7.BuildStepId

import scala.annotation.tailrec
import scala.io.Source

case class BuildStep(id: BuildStepId, mustRunAfter: Set[BuildStepId])

object Day7 extends App {

  type BuildStepId = String

  val buildSteps = Source.fromResource("2018/day7/input.txt").getLines().foldLeft[Map[BuildStepId, BuildStep]](Map.empty) { case (map, rule) =>
    val dependsOn: BuildStepId = rule.substring(5,6)
    val stepName: BuildStepId = rule.substring(36,37)
    val mapWithStep = map.get(stepName) match {
      case None => map + (stepName -> BuildStep(stepName, Set(dependsOn)))
      case Some(bs) => map + (stepName -> bs.copy(mustRunAfter = bs.mustRunAfter + dependsOn))
    }

    if(mapWithStep.contains(dependsOn)) {
      mapWithStep
    } else {
      mapWithStep + (dependsOn -> BuildStep(dependsOn, Set.empty))
    }
  }

  @tailrec
  def solveSteps(openSteps: Map[BuildStepId, BuildStep], orderedStepsDone: List[BuildStepId] = Nil): List[BuildStepId] = {
    if(openSteps.isEmpty){
      orderedStepsDone.reverse
    } else {
      val nextPossibleSteps = openSteps.values.filter(bs => bs.mustRunAfter.count(afterStepId => openSteps.contains(afterStepId)) == 0)
      val nextStep = nextPossibleSteps.toList.minBy(_.id)
      solveSteps(openSteps - nextStep.id, nextStep.id :: orderedStepsDone)
    }
  }


  println(s"Solution 1: ${solveSteps(buildSteps).mkString}")


}
